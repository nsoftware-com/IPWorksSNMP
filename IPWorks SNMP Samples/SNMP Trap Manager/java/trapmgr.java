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

public class trapmgr {

  SNMPAgent agent;
  SNMPTrapMgr trapmgr;

  public trapmgr() {

    try {
      agent = new SNMPAgent();
      agent.setLocalEngineId("MyEngine");

      System.out.println("Starting agent...");
      agent.setAcceptData(true);

      trapmgr = new SNMPTrapMgr();
      trapmgr.addSNMPTrapMgrEventListener(new TrapMgrEvents(this));

      System.out.println("Starting trap manager...");
      trapmgr.setActive(true);

      //Secure traps are available in version 3. You may also choose V1 or V2c, but
      //this demo uses V3 to demonstrate secure traps
      System.out.println("\r\nSending a trap...");
      agent.setSNMPVersion(agent.snmpverV2c);
      agent.sendTrap("255.255.255.255", "coldStart");

      agent.sendSecureTrap("255.255.255.255", "coldStart", "desuser", 1, "des_password", 1, "des_password"); //md5 and des hardcoded here

    } catch (IPWorksSNMPException ex) {
      System.out.println("IPWorks SNMP Exception: " + ex.getMessage());
    } catch (Exception ex) {
      System.out.println("Exception: " + ex.getMessage());
    }
  }
  public static void main(String[] args) {
    new trapmgr();

  }

  public void onTrap (SNMPTrapMgrTrapEvent arg0) {
    System.out.println("\r\n---- Trap received!");
    System.out.println("    Source address: " + arg0.sourceAddress);
    System.out.println("    Trap OID: " + arg0.trapOID);
    System.out.println("    Time stamp: " + String.valueOf(arg0.timeStamp));
  }

  public void onGetUserPassword (SNMPTrapMgrGetUserPasswordEvent arg0) {

    if (arg0.passwordType == 1) {
      System.out.println("\r\nVerifying authentication password for incoming secure trap.");
      System.out.println("    User: " + arg0.user);
      //now, the trap manager checks to see what password is defined for this
      //user and engineid, and sets the password parameter to the event
      //If the password matches what the agent sent, the trap manager
      //continues to process the request, moving on to encryption password,
      //if sent. Otherwise, the message is rejected.
      if (arg0.user.equals("desuser")) {
        arg0.password = "des_password";
      }
    } else //encryption password
    {
      System.out.println("\r\nVerifying encryption password for incoming secure trap.");
      System.out.println("    User: " + arg0.user);
      //the trap maanger attempts to decrypt the trap with the password
      //it has defined for that user. If the trap manager doesn't have the
      //right password define, it can't read the trap.
      //While the authentication password is used to make sure that the
      //right agent has sent the trap, the encryption password makes certain
      //that no unauthorized manager can read the data
      if (arg0.user.equals("desuser")) {
        arg0.password = "des_password";
      }
    }

  }

}

class TrapMgrEvents implements SNMPTrapMgrEventListener{
  trapmgr instance;

  public TrapMgrEvents(trapmgr instance) {
    this.instance = instance;
  }
  public void badPacket(SNMPTrapMgrBadPacketEvent arg0) {}
  public void cacheEntry(SNMPTrapMgrCacheEntryEvent arg0) {}
  public void checkEngine(SNMPTrapMgrCheckEngineEvent arg0) {}
  @Override
  public void connected(SNMPTrapMgrConnectedEvent snmpTrapMgrConnectedEvent) {}
  @Override
  public void disconnected(SNMPTrapMgrDisconnectedEvent snmpTrapMgrDisconnectedEvent) {}
  public void discoveryRequest(SNMPTrapMgrDiscoveryRequestEvent arg0) {}
  public void error(SNMPTrapMgrErrorEvent arg0) {}
  public void getUserPassword(SNMPTrapMgrGetUserPasswordEvent arg0) {
    instance.onGetUserPassword(arg0);
  }
  public void getUserSecurityLevel(SNMPTrapMgrGetUserSecurityLevelEvent arg0) {}
  public void hashPassword(SNMPTrapMgrHashPasswordEvent arg0) {}
  public void informRequest(SNMPTrapMgrInformRequestEvent arg0) {}
  public void packetTrace(SNMPTrapMgrPacketTraceEvent arg0) {}
  @Override
  public void SSLClientAuthentication(SNMPTrapMgrSSLClientAuthenticationEvent snmpTrapMgrSSLClientAuthenticationEvent) {}
  @Override
  public void SSLStatus(SNMPTrapMgrSSLStatusEvent snmpTrapMgrSSLStatusEvent) {}
  public void trap(SNMPTrapMgrTrapEvent arg0) {
    instance.onTrap(arg0);
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



