/* Taken from linux/kernel/cpu.c */
/**
 * DOC: simple
 */

/**
 * cpuhp_invoke_callback - Invoke the callbacks for a given state
 * @cpu:	The cpu for which the callback should be invoked
 * @state:	The state to do callbacks for
 * @bringup:	True if the bringup callback should be invoked
 * @node:	For multi-instance, do a single entry callback for install/remove
 * @lastp:	For multi-instance rollback, remember how far we got
 *
 * Called from cpu hotplug and from the state register machinery.
 *
 * Return: %0 on success or a negative errno code
 */
static int cpuhp_invoke_callback(unsigned int cpu, enum cpuhp_state state,
				 bool bringup, struct hlist_node *node,
				 struct hlist_node **lastp)
{
	return 0;
}

/* Taken from linux/include/linux/acpi.h */
/**
 * ACPI_DEVICE_CLASS - macro used to describe an ACPI device with
 * the PCI-defined class-code information
 *
 * @_cls : the class, subclass, prog-if triple for this device
 * @_msk : the class mask for this device
 *
 * This macro is used to create a struct acpi_device_id that matches a
 * specific PCI class. The .id and .driver_data fields will be left
 * initialized with the default value.
 */
#define ACPI_DEVICE_CLASS(_cls, _msk)	.cls = (_cls), .cls_msk = (_msk),

/* Taken from linux/include/linux/acpi_dma.h */
/**
 * struct acpi_dma_spec - slave device DMA resources
 * @chan_id:	channel unique id
 * @slave_id:	request line unique id
 * @dev:	struct device of the DMA controller to be used in the filter
 *		function
 */
struct acpi_dma_spec {
	int		chan_id;
	int		slave_id;
	struct device	*dev;
};

/* Taken from linux/include/linux/dmaengine.h */
/**
 * typedef dma_cookie_t - an opaque DMA cookie
 *
 * if dma_cookie_t is >0 it's a DMA request cookie, <0 it's an error code
 */
typedef s32 dma_cookie_t;

/* Taken from linux/include/linux/coresight.h */
/**
 * union coresight_dev_subtype - further characterisation of a type
 * @sink_subtype:	type of sink this component is, as defined
 *			by @coresight_dev_subtype_sink.
 * @link_subtype:	type of link this component is, as defined
 *			by @coresight_dev_subtype_link.
 * @source_subtype:	type of source this component is, as defined
 *			by @coresight_dev_subtype_source.
 * @helper_subtype:	type of helper this component is, as defined
 *			by @coresight_dev_subtype_helper.
 * @ect_subtype:        type of cross trigger this component is, as
 *			defined by @coresight_dev_subtype_ect
 */
union coresight_dev_subtype {
	/* We have some devices which acts as LINK and SINK */
	struct {
		enum coresight_dev_subtype_sink sink_subtype;
		enum coresight_dev_subtype_link link_subtype;
	};
	enum coresight_dev_subtype_source source_subtype;
	enum coresight_dev_subtype_helper helper_subtype;
	enum coresight_dev_subtype_ect ect_subtype;
};
