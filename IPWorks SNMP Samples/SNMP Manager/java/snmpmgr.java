/*
 * IPWorks SNMP 2024 Java Edition - Sample Project
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
 */

import java.io.*;

import ipworkssnmp.*;
import java.io.*;

public class snmpmgr {

  SNMPMgr mgr;

  public snmpmgr(String[] args) {
    try {
      if (args.length < 2) {
        System.out.println("Usage: snmpmgr [options] agent oid\r\n");
        System.out.println("Options: ");
        System.out.println("  -v\tThe SNMP version to use (1, 2c, or 3.  Default is 2.)");
        System.out.println("  -c\tThe SNMP community to use (SNMPv1 or 2c only.  Default is \"public\".)");
        System.out.println("  -u\tThe user name to use for authentication (SNMPv3 only)");
        System.out.println("  -a\tThe auth protocol to use (MD5 or SHA, SNMPv3 only.  Default is MD5.)");
        System.out.println("  -A\tThe auth password to use (SNMPv3 only)");
        System.out.println("  -x\tThe enc prot to use (DES, 3DES, or AES, SNMPv3 only.  Default is DES.)");
        System.out.println("  -X\tThe encryption password to use (SNMPv3 only)");
        System.out.println("  -t\tThe timeout value (in seconds.  Default is 60.)");
        System.out.println("\r\nExample: snmpmgr -v 3 -u myuser -A my_password myagent 1.3.6.1.2.1.1.1.0");
        System.out.println("Press enter to continue...");
        input();
        System.exit(0);
      }

      mgr = new SNMPMgr();
      mgr.setActive(true);

      //get the cmd line args
      for (int i=0; i<args.length; i++) {
        if (args[i].startsWith("-")) {
          if (args[i].equals("-v")) mgr.setSNMPVersion(Integer.valueOf(args[i+1]));
          if (args[i].equals("-c")) mgr.setCommunity(args[i+1]);
          if (args[i].equals("-u")) mgr.setUser(args[i+1]);
          if (args[i].equals("-a")) {
            String protocol = args[i+1].toLowerCase();
            if (protocol.equals("md5")) mgr.setAuthenticationProtocol(1);
            else if (protocol.equals("sha")) mgr.setAuthenticationProtocol(2);
          }
          if (args[i].equals("-A")) mgr.setAuthenticationPassword(args[i+1]);
          if (args[i].equals("-x")) {
            String protocol = args[i+1].toLowerCase();
            if (protocol.equals("des")) mgr.setEncryptionAlgorithm(1);
            else if (protocol.equals("aes")) mgr.setEncryptionAlgorithm(2);
            else if (protocol.equals("3des")) mgr.setEncryptionAlgorithm(3);
          }
          if (args[i].equals("-X")) mgr.setEncryptionPassword(args[i+1]);
          if (args[i].equals("-t")) mgr.setTimeout(Integer.valueOf(args[i+1])); //A positive value will ensure that all operations are synchronous
        }
      }
      mgr.setRemoteHost(args[args.length-2]);
      String oid = args[args.length-1];

      //discovering the agent in necessary in SNMP v3 to get the
      //engine id, engine boots, and time of the agent
      //Here, you must set the user, authentication password, and
      //encryption password
      if (mgr.getSNMPVersion() == 3) {
        System.out.println("\r\nDiscovering agent...");
        mgr.discover();
        System.out.println("Discovered agent (engine id " + new String(mgr.getRemoteEngineId()) + ").\r\n");
      }

      mgr.getObjects().clear();
      mgr.getObjects().add(new SNMPObject(oid));
      mgr.sendGetRequest();
      if (mgr.getErrorStatus() == 0) {
        System.out.println(mgr.getObjects().item(0).getOid() + " = " + new String(mgr.getObjects().item(0).getValue()));
      } else {
        System.out.println("Error " + String.valueOf(mgr.getErrorStatus()) + ": " + mgr.getErrorDescription());
      }
      System.out.println("\r\nPress enter to continue...");
      input();
      System.exit(0);

    } catch (IPWorksSNMPException ex) {
      System.out.println("IPWorks SNMP Exception: " + ex.getMessage());
    } catch (Exception ex) {
      System.out.println("Exception: " + ex.getMessage());
    }
  }

  private String input() throws IOException
  {
    BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
    return bf.readLine();
  }

  public static void main(String[] args) {
    new snmpmgr(args);
  }

}

class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksSNMPException) {
      System.out.print(" (" + ((IPWorksSNMPException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



