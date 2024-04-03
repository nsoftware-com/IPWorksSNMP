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

public class snmpmgrDemo
{
    public async static Task Main(string[] args)
    {
        try
        {
            Snmpmgr mgr;

            if (args.Length < 2)
            {
                Console.WriteLine("Usage: snmpmgr [options] agent oid\r\n");
                Console.WriteLine("Options: ");
                Console.WriteLine("  -v\tThe SNMP version to use (1, 2c, or 3.  Default is \"public\")");
                Console.WriteLine("  -c\tThe SNMP community to use (SNMPv1 or 2c only.  Default is \"public\".)");
                Console.WriteLine("  -u\tThe user name to use for authentication (SNMPv3 only)");
                Console.WriteLine("  -a\tThe auth protocol to use (MD5 or SHA, SNMPv3 only.  Default is MD5.)");
                Console.WriteLine("  -A\tThe auth password to use (SNMPv3 only)");
                Console.WriteLine("  -x\tThe enc prot to use (DES, 3DES, or AES, SNMPv3 only.  Default is DES.)");
                Console.WriteLine("  -X\tThe encryption password to use (SNMPv3 only)");
                Console.WriteLine("  -t\tThe timeout value (in seconds.  Default is 60.)");
                Console.WriteLine("\r\nExample: snmpmgr -v 3 -u myuser -A my_password myagent 1.3.6.1.2.1.1.1.0");
                Console.WriteLine("Press enter to continue...");
                Console.Read();
            }
            else 
            {
                mgr = new Snmpmgr();
                await mgr.Activate();

                // console args
                for (int i = 0; i < args.Length; i++)
                {
                    if (args[i].StartsWith("-")) // args[i + 1] is the actual param value
                    {
                        if (args[i].Equals("-v")) mgr.SNMPVersion = (SnmpmgrSNMPVersions)Int32.Parse(args[i + 1]);
                        if (args[i].Equals("-c")) mgr.Community = args[i + 1];
                        if (args[i].Equals("-u")) mgr.User = args[i + 1];
                        if (args[i].Equals("-a"))
                        {
                            String protocol = args[i + 1].ToLower();
                            if (protocol.Equals("md5")) mgr.AuthenticationProtocol = SnmpmgrAuthenticationProtocols.authpHMACMD596;
                            else if (protocol.Equals("sha")) mgr.AuthenticationProtocol = SnmpmgrAuthenticationProtocols.authpHMACSHA96;
                        }
                        if (args[i].Equals("-A")) mgr.AuthenticationPassword = args[i + 1];
                        if (args[i].Equals("-x"))
                        {
                            String protocol = args[i + 1].ToLower();
                            if (protocol.Equals("des")) mgr.EncryptionAlgorithm = SnmpmgrEncryptionAlgorithms.encraDES;
                            else if (protocol.Equals("aes")) mgr.EncryptionAlgorithm = SnmpmgrEncryptionAlgorithms.encraAES;
                            else if (protocol.Equals("3des")) mgr.EncryptionAlgorithm = SnmpmgrEncryptionAlgorithms.encra3DES;
                        }
                        if (args[i].Equals("-X")) mgr.EncryptionPassword = args[i + 1];
                        if (args[i].Equals("-t")) mgr.Timeout = Int32.Parse(args[i + 1]);
                    }
                }

                mgr.RemoteHost = args[args.Length - 2];
                String oid = args[args.Length - 1];

                /* 
                 * Discovering the agent is necessary in SNMP v3 to get the engine id,
                 * engine boots, and time of the agent.
                 * Here, you must set the user, authentication password, and encryption password.
                 */
                if (mgr.SNMPVersion == SnmpmgrSNMPVersions.snmpverV3)
                {
                    Console.WriteLine("\r\nDiscovering agent...");
                    await mgr.Discover();
                    Console.WriteLine("Discovered agent engine id: " + mgr.RemoteEngineId + ".\r\n");
                }

                mgr.Objects.Clear();
                mgr.Objects.Add(new SNMPObject(oid));

                await mgr.SendGetRequest();

                if (mgr.ErrorStatus == 0)
                {
                    Console.WriteLine(mgr.Objects[0].Oid + " = " + mgr.Objects[0].Value);
                }
                else
                {
                    Console.WriteLine("Error: " + mgr.ErrorStatus + ": " + mgr.ErrorDescription);
                }

                Console.WriteLine("\r\nPress enter to continue....");
                Console.Read();
            }
        }
        catch (IPWorksSNMPException e)
        {
            Console.WriteLine("IPWorks SNMP Exception: " + e.Message);
        }
        catch (Exception e) 
        {
            Console.WriteLine("Exception: " + e.Message);
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