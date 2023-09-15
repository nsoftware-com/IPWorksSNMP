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
using System;
using System.Threading.Tasks;
using nsoftware.async.IPWorksSNMP;

class mibbrowserDemo
{
  private static Mibbrowser mibbrowser1 = new nsoftware.async.IPWorksSNMP.Mibbrowser();

  private static void mibbrowser1_OnMibNode(object sender, nsoftware.async.IPWorksSNMP.MibbrowserMibNodeEventArgs e)
  {
    Console.WriteLine(e.NodeOid + ": " + e.NodeLabel);
  }

  static async Task Main(string[] args)
  {
    if (args.Length < 2)
    {
      Console.WriteLine("usage: mcchat mibfile oid");
      Console.WriteLine("  mibfile   path of mib file to browse");
      Console.WriteLine("  oid       oid of node to browse");
      Console.WriteLine("Example: mibbrowser .\\rfc1213-mib ifTable");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {
      try
      {
        string mibfile = args[0];
        string oid = args[1];

        // Load MIB file and select node
        await mibbrowser1.LoadMib(mibfile);
        await mibbrowser1.SelectNode(oid);

        // Display details of node
        Console.WriteLine("Details:");
        Console.WriteLine(mibbrowser1.NodeOid + ": " + mibbrowser1.NodeLabel);
        Console.WriteLine(mibbrowser1.NodeDescription + "\r\n");

        // List successors (through OnMibNode event)
        Console.WriteLine("Successors:");
        mibbrowser1.ListSuccessors();
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
      Console.WriteLine("\npress <return> to continue...");
      Console.Read();
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