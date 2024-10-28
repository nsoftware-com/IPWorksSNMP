/*
 * IPWorks SNMP 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworkssnmp = require("@nsoftware/ipworkssnmp");

if(!ipworkssnmp) {
  console.error("Cannot find ipworkssnmp.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {

  console.log("*******************************************");
  console.log("Usage: node snmpagent.js [options]");
  console.log("  -p\tThe local port. Default is 161.");
  console.log("\nExample: snmpagent -p 7777");
  console.log("*******************************************");

  const agent = new ipworkssnmp.snmpagent();
  const args = process.argv;

  for (let i = 0; i < args.length; i++) {
    if (args[i].indexOf('-') === 0) {
      if (args[i] === "-p") agent.setLocalPort(parseInt(args[i + 1]));
    }
  }

  agent.on('GetRequest', (e) => {
    console.log('Received GetRequest');
    //By default, Respond is false, meaning do NOT answer the request.  
    //So we'll change this to true, so that a response
    //will automatically be sent after the code in this event is completed.
    e.respond = true;
    //for the purposes of a clear demo, we'll loop through and set all of the objtypes and objvalues
    for (let i = 0; i < agent.getObjects().size(); i++) {
      let obj = agent.getObjects().get(i);
      switch (obj.getOid()) {
        case '1.3.6.1.2.1.1.1.0': //sysDescr
          obj.setValue('My System');
          obj.setObjectType(4); //OctetString
          break;

        case '1.3.6.1.2.1.1.2.0':  //sysObjectId
          obj.setValue('1.3.6.1.4.1.127.0.0.1');
          obj.setObjectType(6); //ObjectId
          break;

        case '1.3.6.1.2.1.1.3.0': //sysUpTime
          obj.setValue("" + obj.getSysUpTime());
          obj.setObjectType(67);  //Time Ticks
          break;

        case '1.3.6.1.2.1.1.4.0': //sysContact
          obj.setValue('support@nsoftware.com');
          obj.setObjectType(4); //OctetString
          break;

        case '1.3.6.1.2.1.1.5.0': //sysName
          obj.setValue('localhost');
          obj.setObjectType(4); //OctetString
          break;

        case '1.3.6.1.2.1.1.6.0': //sysLocation
          obj.setValue('(unknown)');
          obj.setObjectType(4); //OctetString
          break;
        default:
          e.errorStatus = 2  //No such name
          break;
      }
    } //next i
  })
    .on('GetNextRequest', (e) => {
      console.log('Received GetNextRequest');
      //By default, Respond is false, meaning do NOT answer the request.  
      //So we'll change this to true, so that a response
      //will automatically be sent after the code in this event is completed.
      e.respond = true;

      //for the purposes of a clear demo, we'll loop through and set all of the objtypes and objvalues
      for (let i = 0; i < agent.getObjects().size(); i++) {
        let obj = agent.getObjects().get(i);
        //set objType and objValue to send get response
        switch (obj.getOid()) {
          case '1.3.6.1.2.1.1.1':
            obj.setOid('1.3.6.1.2.1.1.1.0');//sysDescr
            obj.setValue('My System');
            obj.setObjectType(4); //OctetString
            break;

          case '1.3.6.1.2.1.1.1.0':
            obj.setOid('1.3.6.1.2.1.1.2.0');  //sysObjectId
            obj.setValue('1.3.6.1.4.1.127.0.0.1');
            obj.setObjectType(6); //ObjectId
            break;

          case '1.3.6.1.2.1.1.2.0':
            obj.setOid('1.3.6.1.2.1.1.3.0'); //sysUpTime
            obj.setValue(obj.getSysUpTime());
            obj.setObjectType(67);  //Time Ticks
            break;

          case '1.3.6.1.2.1.1.3.0':
            obj.setOid('1.3.6.1.2.1.1.4.0'); //sysContact
            obj.setValue('support@nsoftware.com');
            obj.setObjectType(4); //OctetString
            break;

          case '1.3.6.1.2.1.1.4.0':
            obj.setOid('1.3.6.1.2.1.1.5.0'); //sysName
            obj.setValue('localhost');
            obj.setObjectType(4); //OctetString
            break;

          case '1.3.6.1.2.1.1.5.0':
            obj.setOid('1.3.6.1.2.1.1.6.0'); //sysLocation
            obj.setValue('(unknown)');
            obj.setObjectType(4); //OctetString
            break;
          default:
            e.errorStatus = 2;  //No such name
            break;
        }
      } //next i    
    });

  agent.setLocalEngineId('MyAgent');
  try {
    await agent.activate();
  } catch (err) {
    console.log(err);
    process.exit();
  }

  console.log('Listening ...');

}


function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
