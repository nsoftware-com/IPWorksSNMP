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
﻿using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorksSNMP;

public class trapmgrDemo
{
    static Snmpagent agent;
    static Snmptrapmgr trapmgr;

    static void Snmptrapmgr_OnTrap(object? sender, SnmptrapmgrTrapEventArgs args)
    {
        Console.WriteLine("\r\n---- Trap received!");
        Console.WriteLine("     Source address: " + args.SourceAddress);
        Console.WriteLine("     Trap OID: " + args.TrapOID);
        Console.WriteLine("     Time stamp: " + args.TimeStamp);
    }

    static void Snmptrapmgr_OnGetUserPassword(object? sender, SnmptrapmgrGetUserPasswordEventArgs args)
    {
        if (args.PasswordType == 1)
        {
            Console.WriteLine("\r\nVerifying authentication password for incoming secure trap.");
            Console.WriteLine("     User: " + args.User);

            // Now, the trap manager checks to see what password is defined for this
            // user and engineid, and sets the password parameter to the event
            // If the password matches what the agent sent, the trap manager
            // continues to process the request, moving on to encryption password,
            // if sent. Otherwise, the message is rejected.
            if (args.User.Equals("desuser"))
            {
                args.Password = "despassword";
            }
        } else // encryption password
        {
            Console.WriteLine("\r\nVerifying encryption password for incoming secure trap.");
            Console.WriteLine("     User: " + args.User);

            // The trap maanger attempts to decrypt the trap with the password
            // it has defined for that user. If the trap manager doesn't have the
            // right password define, it can't read the trap.
            // While the authentication password is used to make sure that the
            // right agent has sent the trap, the encryption password makes certain
            // that no unauthorized manager can read the data.
            if (args.User.Equals("desuser"))
            {
                args.Password = "despassword";
            }
        }
    }

    static async Task Main(string[] args)
    {
        try
        {
            agent = new Snmpagent();
            agent.LocalEngineId = "MyEngine";

            Console.WriteLine("Starting agent...");
            agent.AcceptData = true;

            trapmgr = new Snmptrapmgr();
            trapmgr.OnGetUserPassword += Snmptrapmgr_OnGetUserPassword;
            trapmgr.OnTrap += Snmptrapmgr_OnTrap;

            Console.WriteLine("Starting trap manager...");
            await trapmgr.Activate();

            // Secure traps are available in version 3. You may also choose V1 or V2c, but
            // this demo uses V3 to demonstrate secure traps
            Console.WriteLine("\r\nSending a trap...");
            agent.SNMPVersion = SnmpagentSNMPVersions.snmpverV3;
            await agent.SendTrap("255.255.255.255", "coldStart");

            await agent.SendSecureTrap("255.255.255.255", "coldStart", "desuser", 1, "des_password", 1, "des_password"); // md5 and des hardcoded here

        } catch (IPWorksSNMPException e)
        {
            Console.WriteLine("IPWorks SNMP Exception: " + e.Message);
        } catch (Exception e)
        {
            Console.WriteLine(e.Message);
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