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

import java.io.*;
import ipworkssnmp.*;
import java.io.*;

public class snmpwalk {

  SNMPAgent agent;
  SNMPMgr mgr;

  public snmpwalk() {
    try {
      mgr = new SNMPMgr();
      mgr.setActive(true);
      mgr.setTimeout(10);  //This will ensure that all operations are synchronous

      System.out.print("Please enter the address of the SNMP agent to walk [\"localhost\"]: ");
      String buffer = input();
      if (buffer.length() > 0) mgr.setRemoteHost(buffer);
      else mgr.setRemoteHost("localhost");

      String startingoid = "1.3.6.1.2.1.1";
      System.out.print("Please enter the OID of the table to walk [\"" + startingoid + "\"]: ");
      buffer = input();
      if (buffer.length() > 0) startingoid = buffer;

      //discovering the agent in necessary in SNMP v3 to get the
      //engine id, engine boots, and time of the agent
      //Here, you must set the user, authentication password, and
      //encryption password
      System.out.println("\r\nDiscovering agent...");
      mgr.setSNMPVersion(mgr.snmpverV3);
      mgr.setUser("desuser");
      mgr.setAuthenticationPassword("des_password");
      mgr.setEncryptionPassword("des_password");
      mgr.discover();
      System.out.println("Discovered agent (engine id " + mgr.getRemoteEngineId() + ").");
      System.out.println("Walking table 1.3.6.1.2.1.1:\r\n");
      mgr.walk(startingoid);
      for (int i=0; i<mgr.getObjects().size(); i++) {
        System.out.println(mgr.getObjects().item(i).getOid() + "\t" + new String(mgr.getObjects().item(i).getValue()));
      }
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
    new snmpwalk();
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
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
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
}



