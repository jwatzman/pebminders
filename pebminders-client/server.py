#!/usr/bin/env python

import SimpleHTTPServer
import SocketServer

class ServerHandler(SimpleHTTPServer.SimpleHTTPRequestHandler):

	def do_POST(self):
		self.do_GET()

Handler = ServerHandler
httpd = SocketServer.TCPServer(("", 8000), Handler)
httpd.serve_forever()
