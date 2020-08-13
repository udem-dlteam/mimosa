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
  term_write(cout, (native_string) "PCI Setup V");
  term_write(cout, (native_string) "1.0.2\n");

  // Scan everything
  // Output to console for debugging
  for (uint32 bus = 0; bus < 256; ++bus) {
    for (uint32 device = 0; device < 32; ++device) {
      for (uint8 function = 0; function < 8; ++function) {
        if (pci_device_at(bus, device, function)) {
          debug_write((native_string) "Found device at:");
          debug_write(bus);
          debug_write(device);
          debug_write(function);

          uint32 type_block = pci_read_conf(bus, device, function, 0x08);
          uint8 class_code = (type_block >> 24) & 0xFF;
          uint8 subclass = (type_block >> 16) & 0xFF;
          debug_write(class_code);

          switch (class_code) {
          case PCI_CLASS_MASS_STORAGE: {
            term_write(cout, class_code);
            term_write(cout, (native_string) " : ");
            if (subclass == PCI_SUBCLASS_IDE) {
              term_write(cout, (native_string) "Found IDE\n");
            } else if (subclass == PCI_SUBCLASS_PATA) {
              term_write(cout, (native_string) "Found PATA\n");
            } else if (subclass == PCI_SUBCLASS_SATA) {
              term_write(cout, (native_string) "Found SATA\n");
            } else {
              term_write(cout,
                         (native_string) "Found Mass Storage with subclass: ");
              term_write(cout, subclass);
              term_writeline(cout);
            }
            break;
          }
          case PCI_CLASS_NETWORK: {
            term_write(cout, bus);
            term_write(cout, (native_string) " : ");
            term_write(cout, device);
            term_write(cout, (native_string) " : ");
            term_write(cout, function);
            term_write(cout, (native_string) " : ");
            term_write(cout, (native_string) "Found PCI Network\n");
            break;
          }
          case PCI_CLASS_DISPLAY: {
            term_write(cout, bus);
            term_write(cout, (native_string) " : ");
            term_write(cout, device);
            term_write(cout, (native_string) " : ");
            term_write(cout, function);
            term_write(cout, (native_string) " : ");
            term_write(cout, (native_string) "Found PCI Display\n");
            break;
          }
          case PCI_CLASS_BRIDGE: {
            term_write(cout, bus);
            term_write(cout, (native_string) " : ");
            term_write(cout, device);
            term_write(cout, (native_string) " : ");
            term_write(cout, function);
            term_write(cout, (native_string) " : ");
            term_write(cout, (native_string) "Found PCI Bridge ");
            if (subclass == 0x04) {
              term_write(cout, (native_string) "(PCI-PCI)");
            } else if (subclass == 0x00) {
              term_write(cout, (native_string) "(HOST)");
            }
            term_writeline(cout);
            break;
          }
          default: {
            term_write(cout, class_code);
            term_write(cout, (native_string) " : ");
            term_write(cout, subclass);
            term_writeline(cout);
            break;
          }
          }
        }
      }
    }
  }

  term_write(cout, (native_string) "End of PCI setup\n");
}
