module Top(
    input sysClk,
    input reset,
    input               mtimer_update,
    input               mtimer_interruptPending,
    input               external_update,
    input               external_interruptPending,
    output              ibus_cmd_valid,
    input               ibus_cmd_ready,
    output     [31:0]   ibus_cmd_payload_address,
    input               ibus_rsp_valid,
    output              ibus_rsp_ready,
    input      [31:0]   ibus_rsp_payload_rdata,
    output              dbus_cmd_valid,
    input               dbus_cmd_ready,
    output     [31:0]   dbus_cmd_payload_address,
    output              dbus_cmd_payload_write,
    output     [31:0]   dbus_cmd_payload_wdata,
    output     [3:0]    dbus_cmd_payload_wmask,
    input               dbus_rsp_valid,
    output              dbus_rsp_ready,
    input      [31:0]   dbus_rsp_payload_rdata
);

Pipeline pipeline(
    .clk            (sysClk),
    .reset          (reset),
    .external_update(external_update),
    .external_interruptPending(external_interruptPending),
    .ibus_cmd_valid(ibus_cmd_valid),
    .ibus_cmd_ready(ibus_cmd_ready),
    .ibus_cmd_payload_address(ibus_cmd_payload_address),
    .ibus_rsp_valid(ibus_rsp_valid),
    .ibus_rsp_ready(ibus_rsp_ready),
    .ibus_rsp_payload_rdata(ibus_rsp_payload_rdata),
    .dbus_cmd_valid(dbus_cmd_valid),
    .dbus_cmd_ready(dbus_cmd_ready),
    .dbus_cmd_payload_address(dbus_cmd_payload_address),
    .dbus_cmd_payload_write(dbus_cmd_payload_write),
    .dbus_cmd_payload_wdata(dbus_cmd_payload_wdata),
    .dbus_cmd_payload_wmask(dbus_cmd_payload_wmask),
    .dbus_rsp_valid(dbus_rsp_valid),
    .dbus_rsp_ready(dbus_rsp_ready),
    .dbus_rsp_payload_rdata(dbus_rsp_payload_rdata)
);

endmodule
