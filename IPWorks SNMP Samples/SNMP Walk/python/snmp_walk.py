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

try:
  mgr = SNMPMgr()
  mgr.set_active(True)
  mgr.set_timeout(10)  #This will ensure that all operations are synchronous
  version = 2
  #####

  if len(sys.argv) < 2:
    print("Usage: snmpmgr [options] agent oid\r\n")
    print("Options: ")
    print("  -v\tThe SNMP version to use (1, 2c, or 3.  Default is 2.)")
    print("  -c\tThe SNMP community to use (SNMPv1 or 2c only.  Default is \"public\".)")
    print("  -u\tThe user name to use for authentication (SNMPv3 only)")
    print("  -a\tThe auth protocol to use (MD5 or SHA, SNMPv3 only.  Default is MD5.)")
    print("  -A\tThe auth password to use (SNMPv3 only)")
    print("  -x\tThe enc prot to use (DES, 3DES, or AES, SNMPv3 only.  Default is DES.)")
    print("  -X\tThe encryption password to use (SNMPv3 only)")
    print("  -t\tThe timeout value (in seconds.  Default is 60.)")
    print("\r\nExample: snmpmgr -v 3 -u myuser -A my_password myagent 1.3.6.1.2.1.1.1.0")
    print("Press enter to continue...")
    input()
    sys.exit(0)

  for i in range(len(sys.argv)):
    if sys.argv[i].startswith("-"):
      if sys.argv[i] == "-v":
        mgr.set_snmp_version(int(sys.argv[i+1]))
        version = sys.argv[i+1]
    elif sys.argv[i] == "-c":
      mgr.set_community(sys.argv[i+1])
    elif sys.argv[i] == "-u":
      mgr.set_user(sys.argv[i+1])
    elif sys.argv[i] == "-a":
      protocol = sys.argv[i+1].lower()
      if protocol == "md5":
        mgr.set_authentication_protocol(1)
      elif protocol == "sha":
        mgr.set_authentication_protocol(2)

    elif sys.argv[i] == "-A":
      mgr.set_authentication_password(sys.argv[i+1]);
    elif sys.argv[i] == "-x":
      protocol = sys.argv[i+1].lower()
      if protocol == "des":
        mgr.set_encryption_algorithm(1)
      elif protocol == "aes":
        mgr.set_encryption_algorithm(2)
      elif protocol == "3des":
        mgr.set_encryption_algorithm(3)

    elif sys.argv[i] == "-X":
      mgr.set_encryption_password(sys.argv[i+1])
    elif sys.argv[i] == "-t":
      mgr.set_timeout(sys.argv[i+1]) #A positive value will ensure that all operations are synchronous
#####
  mgr.set_remote_host(sys.argv[len(sys.argv)-2])
  startingoid = sys.argv[len(sys.argv)-1]

  #discovering the agent in necessary in SNMP v3 to get the 
  #engine id, engine boots, and time of the agent
  #Here, you must set the user, authentication password, and
  #encryption password
  if version == 3:
    print("\r\nDiscovering agent...")
    if len(mgr.get_user()) == 0:
      mgr.set_user(input("Enter username:"))
    if len(mgr.get_authentication_password()) == 0:
      mgr.set_authentication_password(input("Enter Authentication Password:"))
    if len(mgr.get_encryption_password()) == 0:
      mgr.set_encryption_password(input("Enter Encryption Password:"))
    mgr.discover();
    print("Discovered agent (engine id " + mgr.get_remote_engine_id() + ").")

  print("Walking table %s:\r\n" % startingoid);
  mgr.walk(startingoid)
  for i in range(mgr.get_obj_count()):
    print(mgr.get_obj_id(i) + "\t" + str(mgr.get_obj_value(i)))
  sys.exit(0)

except IPWorksSNMPError as e:
  print("IPWorks SNMP Exception: %s" % e.message)
except Exception as e:
  print("Exception: %s" % e)


