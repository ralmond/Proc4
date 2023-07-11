<?php
$schemafile="BigStudy.json";
$request =explode('/',$_GET['PATH_INFO']);
$method = strtolower($_SERVER['REQUEST_METHOD');
$
switch($method) {
  case 'get':
    $ecd = new ECDschema($schemafile);
    if (!$ecd->isValid()) {
      http_response_code(500);
    }
    $out = $ecd->getElement($request);
    if (!$out) {
      http_response_code(404);
      break;
    }
    header('Content-Type: application/json;charset=utf-8');
    echo json_encode($out);
    break;
    
  case 'options':
    header('Access-Control-Allow-Credentials: true');
    header('Access-Control-Allow-Headers: Content-Type, Accept, X-Access-Token,access-control-allow-credentials, access-control-allow-headers, access-control-allow-methods, access-control-allow-origin, access-control-max-age, X-Application-Name, X-Request-Time, X-Powered-by');
    header('Access-Control-Allow-Methods: GET, OPTIONS');
    header('Access-Control-Allow-Origin: *');
    header('Access-Control-Max-Age: 1728000');
    header('Content-Length: 0');
    header('Content-Type: text/plain');
    break;
  case 'post':
  case 'put':
  case 'delete':
  default:
    http_response_code(405);
}

function get_ecd_metadata($ecdID) {
  $appStem = array_pop($ecdID);


}


?>
