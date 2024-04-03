/*
 * IPWorks SNMP 2022 .NET Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SNMP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkssnmp
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 * 
 */

using System.Collections.Generic;
﻿using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using nsoftware.async.IPWorksSNMP;

public class snmpDemo
{
    static async Task Main(string[] args)
    {
        Snmpagent agent = new Snmpagent();
        Snmpmgr mgr = new Snmpmgr();

        if (args.Length < 2)
        {
            Console.WriteLine("Usage: snmpwalk remoteHostAddress tableOID");
            Console.WriteLine("Options:");
            Console.WriteLine("  remoteHostAddress the address of the SNMP agent to walk");
            Console.WriteLine("  tableOID the OID of the table to walk");
            Console.WriteLine("Example: snmpwalk localhost 1.3.6.1.2.1.1");
        }
        else
        {
            try
            {
                string remoteHostIP = args[0];
                string tableOid = args[1];

                await mgr.Activate();
                mgr.RemoteHost = remoteHostIP;

                // Discovering the agent is necessary in SNMP v3 to get the engine id, engine boots, and time of the agent
                // Here, you must set the user, authentication password, and encryption password
                Console.WriteLine("Discovering agent...");
                mgr.SNMPVersion = (SnmpmgrSNMPVersions.snmpverV3);
                mgr.User = "desuser";
                mgr.AuthenticationPassword = "des_password";
                mgr.EncryptionPassword = "des_password";

                await mgr.Discover();
                Console.WriteLine("Discovered agent (engine id " + mgr.RemoteEngineId + ").");

                Console.WriteLine("Walking table " + tableOid + "\r\n");
                await mgr.Walk(tableOid);

                foreach (var obj in mgr.Objects)
                {
                    Console.WriteLine(obj.Oid + "\t" + obj.Value);
                }
            }
            catch (IPWorksSNMPException e)
            {
                Console.WriteLine(e.Message);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
    }
}


class ConsoleDemo
{
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}