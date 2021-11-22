module rvfi_wrapper (
    input clock,
    input reset,
    `RVFI_OUTPUTS
);

(* keep *) wire                      ibus_cmd_valid;
(* keep *) `rvformal_rand_reg        ibus_cmd_ready;
(* keep *) wire               [31:0] ibus_cmd_payload_address;
(* keep *) `rvformal_rand_reg        ibus_rsp_valid;
(* keep *) wire                      ibus_rsp_ready;
(* keep *) `rvformal_rand_reg [31:0] ibus_rsp_payload_rdata;
(* keep *) wire                      dbus_cmd_valid;
(* keep *) `rvformal_rand_reg        dbus_cmd_ready;
(* keep *) wire               [31:0] dbus_cmd_payload_address;
(* keep *) wire                      dbus_cmd_payload_write;
(* keep *) wire               [31:0] dbus_cmd_payload_wdata;
(* keep *) wire                [3:0] dbus_cmd_payload_wmask;
(* keep *) `rvformal_rand_reg        dbus_rsp_valid;
(* keep *) wire                      dbus_rsp_ready;
(* keep *) `rvformal_rand_reg [31:0] dbus_rsp_payload_rdata;

Pipeline uut(
    .clk                      (clock),
    .reset                    (reset),
    .ibus_cmd_valid           (ibus_cmd_valid),
    .ibus_cmd_ready           (ibus_cmd_ready),
    .ibus_cmd_payload_address (ibus_cmd_payload_address),
    .ibus_rsp_valid           (ibus_rsp_valid),
    .ibus_rsp_ready           (ibus_rsp_ready),
    .ibus_rsp_payload_rdata   (ibus_rsp_payload_rdata),
    .dbus_cmd_valid           (dbus_cmd_valid),
    .dbus_cmd_ready           (dbus_cmd_ready),
    .dbus_cmd_payload_address (dbus_cmd_payload_address),
    .dbus_cmd_payload_write   (dbus_cmd_payload_write),
    .dbus_cmd_payload_wdata   (dbus_cmd_payload_wdata),
    .dbus_cmd_payload_wmask   (dbus_cmd_payload_wmask),
    .dbus_rsp_valid           (dbus_rsp_valid),
    .dbus_rsp_ready           (dbus_rsp_ready),
    .dbus_rsp_payload_rdata   (dbus_rsp_payload_rdata),
    .mtimer_update            (0),
    .mtimer_interruptPending  (0),
    .external_update          (0),
    .external_interruptPending(0),
    `RVFI_CONN
);

`ifdef RISCV_FORMAL_FAIRNESS
(* keep *) reg [2:0] ibus_cmd_pending_cycles = 0;
(* keep *) reg [2:0] ibus_rsp_pending_cycles = 0;
(* keep *) reg       ibus_rsp_pending_valid = 0;
(* keep *) reg [2:0] dbus_cmd_pending_cycles = 0;
(* keep *) reg [2:0] dbus_rsp_pending_cycles = 0;
(* keep *) reg       dbus_rsp_pending_valid = 0;

wire ibus_rsp_needed = ibus_cmd_valid && ibus_cmd_ready;
wire dbus_rsp_needed = dbus_cmd_valid && dbus_cmd_ready && !dbus_cmd_payload_write;

always @(posedge clock) begin
    if (ibus_cmd_valid && !ibus_cmd_ready) begin
        ibus_cmd_pending_cycles <= ibus_cmd_pending_cycles + 1;
    end else begin
        ibus_cmd_pending_cycles <= 0;
    end

    if (ibus_rsp_pending_valid) begin
        ibus_rsp_pending_cycles <= ibus_rsp_pending_cycles + 1;
    end
    if (ibus_rsp_valid) begin
        ibus_rsp_pending_valid <= 0;
        ibus_rsp_pending_cycles <= 0;
    end
    if (ibus_rsp_needed && !ibus_rsp_valid) begin
        ibus_rsp_pending_valid <= 1;
    end

    if (dbus_cmd_valid && !dbus_cmd_ready) begin
        dbus_cmd_pending_cycles <= dbus_cmd_pending_cycles + 1;
    end else begin
        dbus_cmd_pending_cycles <= 0;
    end

    if (dbus_rsp_pending_valid) begin
        dbus_rsp_pending_cycles <= dbus_rsp_pending_cycles + 1;
    end
    if (dbus_rsp_valid) begin
        dbus_rsp_pending_valid <= 0;
        dbus_rsp_pending_cycles <= 0;
    end
    if (dbus_rsp_needed && !dbus_rsp_valid) begin
        dbus_rsp_pending_valid <= 1;
    end

    // Make sure that cmd acknowledgments or sending of rsp never takes more
    // than 4 cycles. Without an upper bound, liveness can never be guaranteed.
    assume(
        dbus_cmd_pending_cycles < 4 &&
        dbus_rsp_pending_cycles < 4 &&
        ibus_cmd_pending_cycles < 4 &&
        ibus_rsp_pending_cycles < 4
    );

    // These assumptions ensure that the [id]bus_rsp_valid signals are only
    // asserted when a cmd was sent earlier. This forces the formal verification
    // to behave according to the MemBus protocol.
    assume(ibus_rsp_pending_valid || ibus_rsp_needed || !ibus_rsp_valid);
    assume(dbus_rsp_pending_valid || dbus_rsp_needed || !dbus_rsp_valid);
end
`endif

endmodule
