/* Taken from linux/Documentation/doc-guide/kernel-doc.rst */
/**
 * DOC: Typedefs with function prototypes
 */

/**
 * typedef type_name - Brief description.
 * @arg1: description of arg1
 * @arg2: description of arg2
 *
 * Description of the type.
 *
 * Context: Locking context.
 * Return: Meaning of the return value.
 */
 typedef void (*type_name)(struct v4l2_ctrl *arg1, void *arg2);
