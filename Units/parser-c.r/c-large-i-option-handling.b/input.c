/* Taken from linux-3.10.0-229.fc21.x86_64/net/dsa/tag_edsa.c */
struct packet_type edsa_packet_type __read_mostly = {
	.type	= cpu_to_be16(ETH_P_EDSA),
	.func	= edsa_rcv,
};
