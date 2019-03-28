module rvfi_wrapper (
    input clock,
    input reset,
    `RVFI_OUTPUTS
);

wire               [31:0] ibus_address;
wire                      ibus_write;
`rvformal_rand_reg [31:0] ibus_rdata;
wire               [31:0] ibus_wdata;
wire                [3:0] ibus_wmask;
wire               [31:0] dbus_address;
wire                      dbus_write;
`rvformal_rand_reg [31:0] dbus_rdata;
wire               [31:0] dbus_wdata;
wire                [3:0] dbus_wmask;

Pipeline uut(
    .clk            (clock),
    .reset          (reset),
    .ibus_address   (ibus_address),
    .ibus_write     (ibus_write),
    .ibus_rdata     (ibus_rdata),
    .ibus_wdata     (ibus_wdata),
    .ibus_wmask     (ibus_wmask),
    .dbus_address   (dbus_address),
    .dbus_write     (dbus_write),
    .dbus_rdata     (dbus_rdata),
    .dbus_wdata     (dbus_wdata),
    .dbus_wmask     (dbus_wmask),
    `RVFI_CONN
);

endmodule
