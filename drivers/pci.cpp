#include "asm.h"
#include "general.h"
#include "ide.h"
#include "include/pci.h"
#include "rtlib.h"
#include "term.h"

/**
 * Create a config address 32 bit int to write unti a PCI config address
 * register
 */
uint32 make_config_address(bool data, uint8 bus, uint8 device, uint8 func,
                           uint8 offset) {
  // Layout is:
  // 31______30_24____ 23_16___15_11__10-8__7_0
  // EBit    RSVD      BUS     DEVICE FUNC  OFFSET (32 align)
  return (data << 31) | (bus << 16) | (device << 11) | (func << 8) | offset;
}

/**
 * Read the config of the PCIE data
 * Bus and device are pretty self explanatory.
 * Data means wether the ECD bit is set
 * There are 64 registers of 32 bits each. Offset is the
 * byte offset.
 * A device might have multiple roles. Thus the function (8 functions max)
 */
uint32 pci_read_conf(uint8 bus, uint8 device, uint8 func, uint8 offset) {
  uint32 addr = make_config_address(TRUE, bus, device, func, offset);
  outl(addr, PCI_CONFIG_ADDR);
  return inl(PCI_CONFIG_DATA);
}

bool pci_device_at(uint8 bus, uint8 device, uint8 function) {
  /* Layout that exists at this address:
   * DEVICE ID (top 16) | VENDOR ID (bot 16)
   */
  uint32 vendor = pci_read_conf(bus, device, function, 0);
  return vendor != 0xFFFFFFFF; // no device have such vendor
}

void setup_pci() {
  // No detection for now, device drivers will have to scan
}
