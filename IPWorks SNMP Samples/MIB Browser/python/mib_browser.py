# 
# IPWorks SNMP 2022 Python Edition - Sample Project
# 
# This sample project demonstrates the usage of IPWorks SNMP in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
# 
# www.nsoftware.com/ipworkssnmp
# 
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
# 

import sys
import string
from ipworkssnmp import *

input = sys.hexversion<0x03000000 and raw_input or input

def ensureArg(args, prompt, index):
  if len(args) <= index:
    while len(args) <= index:
      args.append(None)
    args[index] = input(prompt)
  elif args[index] == None:
    args[index] = input(prompt)

def fireOnMibNode(e):
  print(e.node_oid + ": " + e.node_label)

try:
  if len(sys.argv) < 3:
    print("Usage: mibbrowser mibfile oid\r\n")
    print("Example: mibbrowser .\\rfc1213-mib ifTable")
    print("Press enter to continue...")
    input()
    sys.exit(0)

  browser = MibBrowser()
  browser.on_mib_node = fireOnMibNode

  # load MIB file and select node
  browser.load_mib(sys.argv[1])
  browser.select_node(sys.argv[2])

  # Display details of node
  print("Details:")
  print(browser.node_oid + ": " + browser.node_label)
  print(browser.node_description + "\r\n")

  # List successors (through OnMIbNode event)
  print("Successors:")
  browser.list_successors()

  print("\r\nPress enter to continue...")
  input()
  sys.exit(0)

except IPWorksSNMPError as e:
  print("IPWorks SNMP Exception: %s" % e.message)
except Exception as e:
  print("Exception: %s" % e)

