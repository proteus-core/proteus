#include "VCore.h"

#include <verilated.h>
#include <verilated_vcd_c.h>

#include <memory>
#include <vector>
#include <iostream>
#include <fstream>

#include <cstdint>
#include <cassert>

class Memory
{
public:

    Memory(VCore& top, const char* memoryFile) : top_{top}
    {
        auto ifs = std::ifstream{memoryFile, std::ifstream::binary};
        auto memoryBytes =
            std::vector<unsigned char>{std::istreambuf_iterator<char>(ifs), {}};

        assert((memoryBytes.size() % 4 == 0) &&
               "Memory does not contain a multiple of words");

        auto i = std::size_t{0};

        while (i < memoryBytes.size())
        {
            auto b0 = memoryBytes[i++];
            auto b1 = memoryBytes[i++];
            auto b2 = memoryBytes[i++];
            auto b3 = memoryBytes[i++];

            auto word = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
            memory_.push_back(word);
        }
    }

    void eval()
    {
        if (top_.ibus_cmd_valid && top_.ibus_rsp_ready) {
            top_.ibus_rsp_valid = true;
            top_.ibus_rsp_payload_rdata = read(top_.ibus_cmd_payload_address);
        }

        if (top_.dbus_cmd_valid && top_.dbus_cmd_payload_write) {
            write(top_.dbus_cmd_payload_address, top_.dbus_cmd_payload_wmask, top_.dbus_cmd_payload_wdata);
        } else {
            if (top_.dbus_rsp_ready) {
                top_.dbus_rsp_valid = true;
                top_.dbus_rsp_payload_rdata = read(top_.dbus_cmd_payload_address);
            }
        }
    }

private:

    using Address = std::uint32_t;
    using Word = std::uint32_t;
    using Mask = std::uint8_t;

    Word read(Address address)
    {
        ensureEnoughMemory(address);
        return memory_[address];
    }

    void write(Address address, Mask mask, Word value)
    {
        ensureEnoughMemory(address);

        auto bitMask = Word{0};
        if (mask & 0x1) bitMask |= 0x000000ff;
        if (mask & 0x2) bitMask |= 0x0000ff00;
        if (mask & 0x4) bitMask |= 0x00ff0000;
        if (mask & 0x8) bitMask |= 0xff000000;

        auto& memoryValue = memory_[address];
        memoryValue &= ~bitMask;
        memoryValue |= value & bitMask;
    }

    void ensureEnoughMemory(Address address)
    {
        if (address >= memory_.size())
        {
            memory_.reserve(address + 1);

            while (address >= memory_.size())
                memory_.push_back(0xcafebabe);
        }
    }

    VCore& top_;
    std::vector<Word> memory_;
};

class CharDev
{
public:

    CharDev(VCore& top) : top_{top}, gotEot_{false}
    {
    }

    void eval()
    {
        if (top_.charOut_valid)
        {
            auto charOut = char(top_.charOut_payload);

            if (charOut == 0x4)
                gotEot_ = true;
            else
            {
                gotEot_ = false;
                std::cout << charOut;
            }
        }
    }

    bool gotEot() const
    {
        return gotEot_;
    }

private:

    VCore& top_;
    bool gotEot_;
};

class TestDev
{
public:

    TestDev(VCore& top) : top_{top}, result_{-1}
    {
    }

    void eval()
    {
        if (top_.testOut_valid)
            result_ = top_.testOut_payload;
    }

    bool gotResult() const
    {
        return result_ >= 0;
    }

    bool hasFailed() const
    {
        return gotResult() && result_ != 0;
    }

    int failedTest() const
    {
        assert(hasFailed() && "No failed tests");
        return result_;
    }

private:

    VCore& top_;
    int result_;
};

int main(int argc, char** argv)
{
    assert(argc >= 2 && "No memory file name given");

    Verilated::commandArgs(argc, argv);

    auto top = std::unique_ptr<VCore>{new VCore};
    top->reset = 1;
    top->clk = 1;

    auto memoryFile = argv[argc - 1];
    auto memory = Memory{*top, memoryFile};
    auto charDev = CharDev{*top};
    auto testDev = TestDev{*top};

    Verilated::traceEverOn(true);
    auto tracer = std::unique_ptr<VerilatedVcdC>{new VerilatedVcdC};
    top->trace(tracer.get(), 99);
    tracer->open("sim.vcd");

    auto mainTime = std::uint64_t{0};
    auto isDone = false;
    int result = 0;

    while (!isDone)
    {
        auto clockEdge = (mainTime % 5 == 0);

        if (clockEdge)
            top->clk = !top->clk;

        if (mainTime > 50)
            top->reset = 0;

        top->eval();

        if (clockEdge && top->clk)
        {
            memory.eval();
            charDev.eval();
            testDev.eval();

            if (charDev.gotEot())
                isDone = true;

            if (testDev.gotResult())
            {
                isDone = true;

                if (testDev.hasFailed())
                {
                    std::cerr << "Test " << testDev.failedTest() << " failed\n";
                    result = 1;
                }
                else
                    std::cout << "All tests passed\n";
            }
        }

        tracer->dump(mainTime);

        mainTime++;
    }

    tracer->close();
    return result;
}
