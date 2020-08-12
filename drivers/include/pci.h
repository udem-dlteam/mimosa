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

void setup_pci();

#endif
