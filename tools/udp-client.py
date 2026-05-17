#!/usr/bin/env python3
"""Quick UDP client for Rampant Factorio mod.

Launch Factorio with: factorio --enable-lua-udp 5200

Usage:
    python3 udp-client.py ping
    python3 udp-client.py info
    python3 udp-client.py squads
    python3 udp-client.py bases
    python3 udp-client.py state
"""

import socket
import json
import sys
import time

FACTORIO_UDP_PORT = 5200  # Port Factorio listens on for incoming packets
RESPONSE_TIMEOUT = 5.0    # Seconds to wait for response


def send_command(cmd):
    """Send a command to Factorio and print the JSON response."""
    # Create a UDP socket with a reusable port for responses
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(('127.0.0.1', 0))  # Bind to random available port
    my_port = sock.getsockname()[1]

    try:
        # Send command to Factorio
        sock.sendto(cmd.encode(), ('127.0.0.1', FACTORIO_UDP_PORT))

        # Wait for response(s)
        sock.settimeout(RESPONSE_TIMEOUT)

        # Handle multi-packet state responses
        if cmd == "state":
            return _recv_state(sock)

        # Single-packet response for other commands
        data, _ = sock.recvfrom(65536)  # Up to 64KB response

        response = json.loads(data.decode())
        print(json.dumps(response, indent=2))
        return response

    except socket.timeout:
        print(f"Timeout waiting for response (Factorio running with --enable-lua-udp {FACTORIO_UDP_PORT}?)")
        return None
    except json.JSONDecodeError as e:
        print(f"Invalid JSON response: {e}")
        return None
    finally:
        sock.close()


def _recv_state(sock):
    """Receive and reassemble multi-packet state response."""
    packets = {}
    req_id = None
    total_packets = None
    tick = None

    while True:
        try:
            data, _ = sock.recvfrom(65536)
            pkt = json.loads(data.decode())

            if 'id' in pkt and 'seq' in pkt:
                # Multi-packet state response
                if req_id is None:
                    req_id = pkt['id']
                if pkt['id'] != req_id:
                    continue  # skip stale packets from other requests

                total_packets = pkt['total']
                seq = pkt['seq']
                packets[seq] = pkt.get('chunks', [])
                if tick is None and 'tick' in pkt:
                    tick = pkt['tick']

                if len(packets) == total_packets:
                    break
            else:
                # Fallback: single-packet response (old format)
                print(json.dumps(pkt, indent=2))
                return pkt
        except socket.timeout:
            break

    # Reassemble chunks in order
    all_chunks = []
    for seq in sorted(packets.keys()):
        all_chunks.extend(packets[seq])

    response = {'chunks': all_chunks, 'count': len(all_chunks)}
    if tick is not None:
        response['tick'] = tick
    print(json.dumps(response, indent=2))
    return response


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    cmd = sys.argv[1]
    response = send_command(cmd)

    if response:
        if cmd == "ping":
            print(f"\n✓ Connected! Tick: {response.get('tick', '?')}")
        elif cmd == "info":
            print(f"\n✓ Awake: {response.get('awake')}")
            print(f"  Evolution: {response.get('evolution')}")
            print(f"  Surfaces: {response.get('surfaces')}")
        elif cmd == "squads":
            print(f"\n✓ Squads: {response.get('count', 0)}")
        elif cmd == "bases":
            print(f"\n✓ Bases: {response.get('count', 0)}")
        elif cmd == "state":
            print(f"\n✓ Chunks: {response.get('count', 0)}")


if __name__ == "__main__":
    main()
