<html>
<head><title>Physics Playground Proc4 Server Status</title>
<?php
    include 'config.php';

    $mong = new MongoDB\Client("mongodb://localhost"); // connect

?>
</head>
<body>

<h1>Processes Currently Running</h1>

<table border="1">
  <tr><th>Application</th><th colspan="2">Evidence Identification</th>
    <th colspan="2">Evidence Accumulation</th>
    <th colspan="2">Activity Selection</th></tr>
  <tr><th></th><th>Active</th><th>Signal</th><th>Active</th><th>Signal</th>
    <th>Active</th><th>Signal</th></tr>
<?php
        foreach ($INI['apps'] as $short => $id) {
                $appQuery = array ('app' => $id);
                $appRec =$mong->Proc4->AuthorizedApps->findOne($appQuery);
?>
<tr>
   <td><?php echo $short; ?></td>
   <td><?php echo $appRec['EIactive']; ?></td>
   <td><?php echo $appRec['EIsignal']; ?></td>
   <td><?php echo $appRec['EAactive']; ?></td>
   <td><?php echo $appRec['EAsignal']; ?></td>
   <td><?php echo $appRec['ASactive']; ?></td>
   <td><?php echo $appRec['ASsignal']; ?></td>


</tr>
<?php 
}
?>
</table>

<?php
/* Old Method */
/* I'm currently having difficulty figuring out how to run shell
 * commands to check if the various proceses are running.  My current
 * strategy is to run the script p4Status as a cron job sending it's
 * output to P4Status.html in the current directory.  */
//include("P4Status.html"); 


?>

<h1>Processed and unprocessed data </h1>

<table border="1">
<tr><th>Application</th><th>Active</th><th>Players</th>
        <th colspan="2">Evidence Identification</th><th colspan="2">Evidence Accumulation</th></tr>
<tr><th></th><th></th><th></th>
        <th>Done</th><th>Left</th><th>Done</th><th>Left</th></tr>
<?php
        foreach ($INI['apps'] as $short => $id) {
                $appQuery = array ('app' => $id);
                $appRec =$mong->Proc4->AuthorizedApps->findOne($appQuery);
                $playerCount = $mong->Proc4->Players->count($appQuery);
                $EIdone = $mong->EIRecords->Events->count(['app'=>$id,'processed' => true]);
                $EIleft = $mong->EIRecords->Events->count(['app'=>$id,'processed' => false]);
                $EAdone = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => true]);
                $EAleft = $mong->EARecords->EvidenceSets->count(['app'=>$id,'processed' => false]);
        
?>
<tr>
   <td><?php echo $short; ?></td>
   <td><?php echo $appRec['active']; ?></td>
   <td><?php echo $playerCount; ?></td>
   <td><?php echo $EIdone; ?></td>
   <td><?php echo $EIleft; ?></td>
   <td><?php echo $EAdone; ?></td>
   <td><?php echo $EAleft; ?></td>
</tr>
<?php 
}
?>
</table>

<h1>Links to results files</h1>

<h3>Observables</h3>
<ul>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.linear.csv">
          linear </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.adaptive.csv">
          adaptive </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppObs.userControl.csv">
          userControl </a></li>
</ul>

<h3>Learning Support Observables</h3>
<ul>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppLS.linear.csv">
          linear </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppLS.adaptive.csv">
          adaptive </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/ppLS.userControl.csv">
          userControl </a></li>
</ul>

<h3>Bayes net scores</h3>
<ul>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/stats.linear.csv">
          linear </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/stats.adaptive.csv">
          adaptive </a></li>
  <li> <a href="https://pluto.coe.fsu.edu/PhysicsPlayground/stats.userControl.csv">
          userControl </a></li>
</ul>
        
</body>
</html>
