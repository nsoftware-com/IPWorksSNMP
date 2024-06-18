<?php
/*
 * IPWorks SNMP 2024 PHP Edition - Sample Project
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
require_once('../include/ipworkssnmp_snmpmgr.php');
require_once('../include/ipworkssnmp_const.php');
?>
<?php
 $snmpmgr = new IPWorksSNMP_SNMPMgr();
?>

<form method=POST>
<center>
<table width="90%">

 <tr><td>Host/Device Address: <td><input type=text name=device value="<?php echo isset($_POST["device"])?$_POST["device"]:""; ?>" size=50>
 <tr><td>Max. Objects:        <td><input type=text name=max value="100">

 <tr><td><td><input type=submit value="   Go!   ">

</table>
</center>
</form>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

  echo "<hr><pre>";

  if ($_POST["device"] == "") {
    echo "<font color=red>Error: Please specify a valid host/device address.</font>";
  } else {

    $snmpmgr->setRemoteHost($_POST["device"]);
    $snmpmgr->setObjCount(1);
    $snmpmgr->setObjId(0,"1.3.6.1.2.1.1.1.0");
    $snmpmgr->setTimeout(10); //wait at most 10 seconds

    $max = $_POST["max"];
    for($count = 1; $count <= $max; $count++){

      //ask the device for one more object
      $snmpmgr->doSendGetNextRequest();

      //print out the response
      if ($snmpmgr->getObjCount() > 0) {
        echo " Index: " . $count . "\t  OID: " . $snmpmgr->getObjId(0) ."<br>";
        echo "\t\tValue: " . htmlspecialchars($snmpmgr->getObjValue(0)) . "<br>";
      }
    }
    echo "</pre>";
   }
}
?>



