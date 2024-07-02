# Taken from linux/tools/power/acpi/tools/pfrut/Makefile
# Don't capture object starting from $.
install-man: $(srctree)/man/pfrut.8
	$(ECHO) "  INST    " pfrut.8
	$(QUIET) $(INSTALL_DATA) -D $< $(DESTDIR)$(mandir)/man8/pfrut.8

# Taken from linux/arch/loongarch/Makefile
# Don't capture object starting from -.h
install:
	$(Q)install -D -m 755 $(KBUILD_IMAGE) $(INSTALL_PATH)/$(image-name-y)-$(KERNELRELEASE)
	$(Q)install -D -m 644 .config $(INSTALL_PATH)/config-$(KERNELRELEASE)
	$(Q)install -D -m 644 System.map $(INSTALL_PATH)/System.map-$(KERNELRELEASE)
