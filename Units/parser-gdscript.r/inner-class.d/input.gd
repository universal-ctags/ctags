# Taken from godot-demo-projects/networking/webrtc_signaling/server/ws_webrtc_server.gd
# of https://github.com/godotengine/godot-demo-projects.git

extends Node

const TIMEOUT = 1000 # Unresponsive clients times out after 1 sec
const SEAL_TIME = 10000 # A sealed room will be closed after this time
const ALFNUM = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

var _alfnum = ALFNUM.to_ascii()

var rand: RandomNumberGenerator = RandomNumberGenerator.new()
var lobbies: Dictionary = {}
var server: WebSocketServer = WebSocketServer.new()
var peers: Dictionary = {}

class Peer extends Reference:
	var id = -1
	var lobby = ""
	var time = OS.get_ticks_msec()

	func _init(peer_id):
		id = peer_id



class Lobby extends Reference:
	var peers: Array = []
	var host: int = -1
	var sealed: bool = false
	var time = 0

	func _init(host_id: int):
		host = host_id

	func join(peer_id, server) -> bool:
		if sealed: return false
		if not server.has_peer(peer_id): return false
		var new_peer: WebSocketPeer = server.get_peer(peer_id)
		new_peer.put_packet(("I: %d\n" % (1 if peer_id == host else peer_id)).to_utf8())
		for p in peers:
			if not server.has_peer(p):
				continue
			server.get_peer(p).put_packet(("N: %d\n" % peer_id).to_utf8())
			new_peer.put_packet(("N: %d\n" % (1 if p == host else p)).to_utf8())
		peers.push_back(peer_id)
		return true


	func leave(peer_id, server) -> bool:
		if not peers.has(peer_id): return false
		peers.erase(peer_id)
		var close = false
		if peer_id == host:
			# The room host disconnected, will disconnect all peers.
			close = true
		if sealed: return close
		# Notify other peers.
		for p in peers:
			if not server.has_peer(p): return close
			if close:
				# Disconnect peers.
				server.disconnect_peer(p)
			else:
				# Notify disconnection.
				server.get_peer(p).put_packet(("D: %d\n" % peer_id).to_utf8())
		return close


	func seal(peer_id, server) -> bool:
		# Only host can seal the room.
		if host != peer_id: return false
		sealed = true
		for p in peers:
			server.get_peer(p).put_packet("S: \n".to_utf8())
		time = OS.get_ticks_msec()
		return true


# Taken from godot-demo-projects/2d/physics_tests/test.gd
class Circle2D:
	extends Node2D
	var center
	var radius
	var color

	func _draw():
		draw_circle(center, radius, color)

func _init():
	server.connect("data_received", self, "_on_data")
	server.connect("client_connected", self, "_peer_connected")
	server.connect("client_disconnected", self, "_peer_disconnected")


# ...
