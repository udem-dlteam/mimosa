#ifndef __PCI_H
#define __PCI_H

#include "general.h"

#define PCI_CONFIG_ADDR (0xCF8)
#define PCI_CONFIG_DATA (0xCFC)

#define PCI_CLASS_MASS_STORAGE (0x01)
#define PCI_CLASS_NETWORK (0x02)
#define PCI_CLASS_DISPLAY (0x03)
#define PCI_CLASS_BRIDGE (0x06)

#define PCI_SUBCLASS_IDE (0x01)
#define PCI_CLASS_IDE_ISA_COMPAT 0x00 // ISA Compatibility mode-only controller
#define PCI_CLASS_IDE_NATIVE 0x05     // PCI native mode - only controller
#define PCI_CLASS_IDE_ISA_BOTH_PCI                                             \
  0x0A //  ISA Compatibility mode controller, supports both channels switched to
       //  PCI native mode
#define PCI_CLASS_IDE_PCI_BOTH_ISA                                             \
  0x0F // PCI native mode controller, supports both channels switched to ISA
       // compatibility mode

#define PCI_SUBCLASS_PATA (0x05)
#define PCI_SUBCLASS_SATA (0x06)

#define PCI_BUS_COUNT 256
#define PCI_DEV_PER_BUS 32
#define PCI_FUNC_PER_DEVICE 8
#define PCI_HEADER_MANUFACTURER_OFFSET 0x00
#define PCI_HEADER_PCI_INFO_OFFSET 0x0C
#define PCI_HEADER_INFO_OFFSET 0x08
#define PCI_HEADER_0_BAR0 0x10
#define PCI_HEADER_0_BAR1 0x14
#define PCI_HEADER_0_BAR2 0x18
#define PCI_HEADER_0_BAR3 0x1C
#define PCI_HEADER_0_BAR4 0x20
#define PCI_HEADER_0_BAR5 0x24
#define PCI_HEADER_0_INT_OFFSET 0x3C
#define PCI_HEADER_LINE_MULTI_FUNCTION (1 << 7)

bool pci_device_at(uint8 bus, uint8 device, uint8 function);

uint32 pci_read_conf(uint8 bus, uint8 device, uint8 func, uint8 offset);

void setup_pci();

#endif
