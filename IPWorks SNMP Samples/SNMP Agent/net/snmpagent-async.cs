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

public class snmpagentDemo
{
    private static Snmpagent agent;
    private static string sysDescr = "My System";
    private static string sysObjectId = "1.3.6.1.4.1.127.0.0.1";
    private static string sysContact = "/n software: sales@nsoftware.com";
    private static string sysName = "localhost";
    private static string sysLocation = "(unknown)";

    private static void Snmpagent_OnBadPacket(object? sender, SnmpagentBadPacketEventArgs e)
    {
        Console.WriteLine("Bad packet received from " + e.SourceAddress + ":" + e.SourcePort);
        Console.WriteLine("Error " + e.ErrorCode + ": " + e.ErrorDescription);
        e.Report = true;
    }

    private static void Snmpagent_OnDiscoveryRequest(object? sender, SnmpagentDiscoveryRequestEventArgs e)
    {
        Console.WriteLine("DiscoveryRequest fired");
        e.Respond = true;
    }

    private static void Snmpagent_OnError(object? sender, SnmpagentErrorEventArgs e)
    {
        Console.WriteLine("Error " + e.ErrorCode + ": " + e.Description);
    }

    private static void Snmpagent_OnGetNextRequest(object? sender, SnmpagentGetNextRequestEventArgs e)
    {
        // This demo assumes all users have permissions to perform a GET action
        e.Respond = true;

        foreach (var obj in agent.Objects)
        {
            if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.1.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.1.0";
                obj.Value = sysDescr;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            else if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.2.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.2.0";
                obj.Value = sysObjectId;
                obj.ObjectType = SNMPObjectTypes.otObjectId;
            }
            else if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.3.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.3.0";
                obj.Value = agent.SysUpTime.ToString();
                obj.ObjectType = SNMPObjectTypes.otTimeTicks;
            }
            else if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.4.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.4.0";
                obj.Value = sysContact;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            else if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.5.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.5.0";
                obj.Value = sysName;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            else if (string.Compare(obj.Oid, "1.3.6.1.2.1.1.6.0") < 0)
            {
                obj.Oid = "1.3.6.1.2.1.1.6.0";
                obj.Value = sysLocation;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            else
            {
                obj.Oid = "1.3.6.1.2.1.2";
                obj.Value = "end";
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
        }
    }

    private static void Snmpagent_OnGetRequest(object? sender, SnmpagentGetRequestEventArgs e)
    {
        // This demo assumes all users have permissions to perform a GET action
        e.Respond = true;

        foreach (var obj in agent.Objects)
        {
            if (obj.Oid == "1.3.6.1.2.1.1.1.0")
            {
                obj.Value = sysDescr;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            if (obj.Oid == "1.3.6.1.2.1.1.2.0")
            {
                obj.Value = sysObjectId;
                obj.ObjectType = SNMPObjectTypes.otObjectId;
            }
            if (obj.Oid == "1.3.6.1.2.1.1.3.0")
            {
                obj.Value = agent.SysUpTime.ToString();
                obj.ObjectType = SNMPObjectTypes.otTimeTicks;
            }
            if (obj.Oid == "1.3.6.1.2.1.1.4.0")
            {
                obj.Value = sysContact;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            if (obj.Oid == "1.3.6.1.2.1.1.5.0")
            {
                obj.Value = sysName;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
            if (obj.Oid == "1.3.6.1.2.1.1.6.0")
            {
                obj.Value = sysLocation;
                obj.ObjectType = SNMPObjectTypes.otOctetString;
            }
        }
    }
    
    private static void Snmpagent_OnGetUserPassword(object? sender, SnmpagentGetUserPasswordEventArgs e)
    {
        Console.WriteLine("User: " + e.User + " password: " + e.Password + " passtype: " + e.PasswordType);
    }

    private static void Snmpagent_OnGetUserSecurityLevel(object? sender, SnmpagentGetUserSecurityLevelEventArgs e)
    {
        /* 
         * This demo uses an internal cache of users so the component handles this automatically
         * If you do not use an internal cache, you must control this yourself, where you would retrieve
         * the user's security level
         */
        return;
    }

    private static void Snmpagent_OnPacketTrace(object? sender, SnmpagentPacketTraceEventArgs e)
    {
        if (e.Direction == 1)
        {
            Console.WriteLine("Packet received from: " + e.PacketAddress + ":" + e.PacketPort);
        }
        else if (e.Direction == 2)
        {
            Console.WriteLine("Packet sent to: " + e.PacketAddress + ":" + e.PacketPort);
        }
    }

    private static void Snmpagent_OnSetRequest(object? sender, SnmpagentSetRequestEventArgs e)
    {
        // This demo assumes all users have permissions to perform a SET action
        return;
    }

    public static async Task Main(string[] args)
    {
        try
        {
            agent = new Snmpagent();

            agent.OnBadPacket += Snmpagent_OnBadPacket;
            agent.OnDiscoveryRequest += Snmpagent_OnDiscoveryRequest;
            agent.OnError += Snmpagent_OnError;
            agent.OnGetNextRequest += Snmpagent_OnGetNextRequest;
            agent.OnGetRequest += Snmpagent_OnGetRequest;
            agent.OnGetUserPassword += Snmpagent_OnGetUserPassword;
            agent.OnGetUserSecurityLevel += Snmpagent_OnGetUserSecurityLevel;
            agent.OnPacketTrace += Snmpagent_OnPacketTrace;
            agent.OnSetRequest += Snmpagent_OnSetRequest;

            agent.LocalEngineId = "MyEngineId";

            Console.WriteLine("This is the SNMPAgent demo for IP*Works! SNMP 2022 .NET Edition \r\n");
            Console.WriteLine("Options:");
            Console.WriteLine("  -v\tThe SNMP version to use (1, 2c, or 3.  Default is 2.)");
            Console.WriteLine("  -p\tThe local port to use (Default is 161)");
            Console.WriteLine("  -u\tThe user name to use for authentication (SNMPv3 only)");
            Console.WriteLine("  -a\tThe auth protocol to use (MD5 or SHA, SNMPv3 only.  Default is MD5.)");
            Console.WriteLine("  -A\tThe auth password to use (SNMPv3 only)");
            Console.WriteLine("  -x\tThe enc prot to use (DES, 3DES, or AES, SNMPv3 only.  Default is DES.)");
            Console.WriteLine("  -X\tThe encryption password to use (SNMPv3 only)");
            Console.WriteLine("  -t\tThe timeout value (in seconds.  Default is 60.)");
            Console.WriteLine("\r\nExample: snmpagent -v 3 -u myuser -A my_password");

            // get arg values
            string user = "";
            int authenticationProtocol = 2; // 1 = md5, 2 = sha
            string authenticationPassword = "";
            int encryptionAlgorithm = 2; // 1 = des, 2 = aes, 3 = 3des
            string encryptionPassword = "";

            for (int i = 0; i < args.Length; i++)
            {
                if (args[i].StartsWith("-"))
                {
                    if (args[i].Equals("-v")) agent.SNMPVersion = (SnmpagentSNMPVersions)Int32.Parse(args[i + 1]);
                    if (args[i].Equals("-p")) agent.LocalPort = Int32.Parse(args[i + 1]);
                    if (args[i].Equals("-u")) user = args[i + 1];
                    if (args[i].Equals("-a"))
                    {
                        string protocol = args[i + 1].ToLower();
                        if (protocol.Equals("md5")) authenticationProtocol = 1;
                        else if (protocol.Equals("sha")) authenticationProtocol = 2;
                    }
                    if (args[i].Equals("-A")) authenticationPassword = args[i + 1];
                    if (args[i].Equals("-x"))
                    {
                        string protocol = args[i + 1].ToLower();
                        if (protocol.Equals("des")) encryptionAlgorithm = 1;
                        if (protocol.Equals("aes")) encryptionAlgorithm = 2;
                        if (protocol.Equals("3des")) encryptionAlgorithm = 3;
                    }
                    if (args[i].Equals("-X")) encryptionPassword = args[i + 1];
                }
            }

            if (user != "")
            {
                await agent.AddUser(user, authenticationProtocol, authenticationPassword, encryptionAlgorithm, encryptionPassword);
            }

            await agent.Activate();

            Console.WriteLine("\r\nSNMP Agent running on port " + agent.LocalPort + ". Press CTRL + C to exit.\r\n");

            while (true)
            {
                await agent.DoEvents();
            }

        }
        catch (IPWorksSNMPException e)
        {
            Console.WriteLine("IPWorksSNMP Exception: " + e.Message);
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