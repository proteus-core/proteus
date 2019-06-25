module rvfi_wrapper (
    input clock,
    input reset,
    `RVFI_OUTPUTS
);

wire                      ibus_cmd_valid;
wire                      ibus_cmd_ready;
wire               [31:0] ibus_cmd_payload_address;
wire                      ibus_rsp_valid;
wire                      ibus_rsp_ready;
`rvformal_rand_reg [31:0] ibus_rsp_payload_rdata;
wire                      dbus_cmd_valid;
wire                      dbus_cmd_ready;
wire               [31:0] dbus_cmd_payload_address;
wire                      dbus_cmd_payload_write;
wire               [31:0] dbus_cmd_payload_wdata;
wire                [3:0] dbus_cmd_payload_wmask;
wire                      dbus_rsp_valid;
wire                      dbus_rsp_ready;
`rvformal_rand_reg [31:0] dbus_rsp_payload_rdata;

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
    `RVFI_CONN
);

endmodule
