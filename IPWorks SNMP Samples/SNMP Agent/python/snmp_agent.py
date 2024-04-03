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

agent = SNMPAgent()
      
sysDescr = "My System"
sysObjectId = "1.3.6.1.4.1.127.0.0.1"
sysContact = "/n software: sales@nsoftware.com"
sysName = "localhost"
sysLocation = "(unknown)"

def fireBadPacket(e):
  print("Bad packet received from " + e.source_address + ":" + str(e.source_port))
  print("Error " + str(e.error_code) + ": " + e.error_description)
  e.report = True

def fireDiscoveryRequest(e):
  print("DicoveryRequest fired")
  e.respond = True

def fireError(e):
  print("Error " + str(e.error_code) + ": " + e.description)

def fireGetNextRequest(e):
  #This demo assumes all users have permissions to perform a GET action
  e.respond = True
  
  for i in range(agent.get_obj_count()):
    if agent.get_obj_id(i) < "1.3.6.1.2.1.1.1.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.1.0")
      agent.set_obj_value(i, sysDescr)
      agent.set_obj_type(i, 4) #Octet String
    elif agent.get_obj_id(i) < "1.3.6.1.2.1.1.2.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.2.0")
      agent.set_obj_value(i, sysObjectId)
      agent.set_obj_type(i, 6) #Object ID
    elif agent.get_obj_id(i) < "1.3.6.1.2.1.1.3.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.3.0")
      agent.set_obj_value(i, str(agent.get_sys_up_time()))
      agent.set_obj_type(i, 67) #Time Ticks
    elif agent.get_obj_id(i) < "1.3.6.1.2.1.1.4.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.4.0")
      agent.set_obj_value(i, sysContact)
      agent.set_obj_type(i, 4) #Octet String
    elif agent.get_obj_id(i) < "1.3.6.1.2.1.1.5.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.5.0")
      agent.set_obj_value(i, sysName)
      agent.set_obj_type(i, 4) #Octet String
    elif agent.get_obj_id(i) < "1.3.6.1.2.1.1.6.0":
      agent.set_obj_id(i, "1.3.6.1.2.1.1.6.0")
      agent.set_obj_value(i, sysLocation)
      agent.set_obj_type(i, 4) #Octet String
    else:
      agent.set_obj_id(i, "1.3.6.1.2.1.2")
      agent.set_obj_value(i, "end")
      agent.set_obj_type(i, 4) #Octet String

def fireGetRequest(e):
  #This demo assumes all users have permissions to perform a GET action
  e.respond = True
  
  for i in range(agent.get_obj_count()):
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.1.0":
      agent.set_obj_value(i, sysDescr)
      agent.set_obj_type(i, 4) #Octet String
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.2.0":
      agent.set_obj_value(i, sysObjectId)
      agent.set_obj_type(i, 6) #Object ID
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.3.0":
      agent.set_obj_value(i, str(agent.get_sys_up_time()))
      agent.set_obj_type(i, 67) #Time Ticks
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.4.0":
      agent.set_obj_value(i, sysContact)
      agent.set_obj_type(i, 4) #Octet String
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.5.0":
      agent.set_obj_value(i, sysName)
      agent.set_obj_type(i, 4) #Octet String
    if agent.get_obj_id(i) == "1.3.6.1.2.1.1.6.0":
      agent.set_obj_value(i, sysLocation)
      agent.set_obj_type(i, 4) #Octet String

def fireGetUserPassword(e):
  print("User: " + e.user + " password: " + e.password + " passtype: " + e.password_type)

def fireGetUserSecurityLevel(e):
  #this demo uses an internal cache of users so the component handles this automatically
  #if you do not use an internal cache, you must control this yourself, where you would
  #retrieve the user's security level
  pass

def firePacketTrace(e):
  if e.direction == 1:
    print("Packet received from: " + e.packet_address + ":" + str(e.packet_port))
  elif e.direction == 2:
    print("Packet sent to: " + e.packet_address + ":" + str(e.packet_port))

def fireSetRequest(e):
  #This demo assumes all users have permissions to perform a SET action
  pass


try:
  print("This is the SNMPAgent demo for IP*Works! SNMP 2022 Python Edition. \r\n")
  print("Options: ")
  print("  -v\tThe SNMP version to use (1, 2c, or 3.  Default is 2.)")
  print("  -p\tThe local port to use (Default is 161)")
  print("  -u\tThe user name to use for authentication (SNMPv3 only)")
  print("  -a\tThe auth protocol to use (MD5 or SHA, SNMPv3 only.  Default is MD5.)")
  print("  -A\tThe auth password to use (SNMPv3 only)")
  print("  -x\tThe enc prot to use (DES, 3DES, or AES, SNMPv3 only.  Default is DES.)")
  print("  -X\tThe encryption password to use (SNMPv3 only)")
  print("  -t\tThe timeout value (in seconds.  Default is 60.)")
  print("\r\nExample: snmpagent -v 3 -u myuser -A my_password")

  agent.on_bad_packet = fireBadPacket
  agent.on_discovery_request = fireDiscoveryRequest
  agent.on_error = fireError
  agent.on_get_next_request = fireGetNextRequest
  agent.on_get_request = fireGetRequest
  agent.on_get_user_password = fireGetUserPassword
  agent.on_get_user_security_level = fireGetUserSecurityLevel
  agent.on_packet_trace = firePacketTrace
  agent.on_set_request = fireSetRequest
  agent.set_local_engine_id("MyEngineID")

  #get the cmd line args
  user = ""
  auth_protocol = 2
  auth_password = ""
  encrypt_algorithm = 2
  encrypt_password = ""
  for i in range(len(sys.argv)):
    if sys.argv[i].startswith("-"):
      if sys.argv[i] == "-v":
        agent.set_snmp_version(int(sys.argv[i+1]))

      if sys.argv[i] == "-p":
        agent.set_local_port(int(sys.argv[i+1]))

      elif sys.argv[i] == "-u":
        user = sys.argv[i+1]

      elif sys.argv[i] == "-a":
        protocol = sys.argv[i+1].lower()
        if protocol == "md5":
            auth_protocol = 1
        elif protocol == "sha":
            auth_protocol = 2

      elif sys.argv[i] == "-A":
        auth_password = sys.argv[i+1]

      elif sys.argv[i] == "-x":
        protocol = sys.argv[i+1].lower()
        if protocol == "des":
          encryptalgorithm = 1
        elif protocol == "aes":
          encryptalgorithm = 2
        elif protocol == "3des":
          encryptalgorithm = 3

      elif sys.argv[i] == "-X":
        encrypt_password = sys.argv[i+1]

      elif sys.argv[i] == "-t":
        agent.set_timeout(sys.argv[i+1]) #A positive value will ensure that all operations are synchronous

  if (user != ""):
    agent.add_user(user, auth_protocol, auth_password, encrypt_algorithm, encrypt_password)
  agent.set_active(True)

  print("\r\nSNMP Agent running on port %d. Press CTRL+C to exit.\r\n" % agent.get_local_port());
  while (True):
    agent.do_events();


except KeyboardInterrupt:
  print("Done.")
except IPWorksSNMPError as e:
  print("IPWorks SNMP Exception: %s" % e.message)
except Exception as e:
  print("Exception: %s"  % e)


