<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks SNMP 2022 Demos - SNMP Manager</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks SNMP 2022 Demos - SNMP Manager">
</head>

<body>

<div id="content">
<h1>IPWorks SNMP - Demo Pages</h1>
<h2>SNMP Manager</h2>
<p>Shows how to create a basic SNMP manager that will find and query SNMP agents for their system information.  The manager demo will also receive SNMP traps.  Also see the TrapMgr component.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
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




<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks SNMP objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-INPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks SNMP 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-INPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
