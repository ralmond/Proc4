<?php

if (false) {
  die("I dont know why I need this to turn on coloring.");
 }

/**
 * A wrapper for the ECDschema.
 * isValid() returns false if there is a problem with reading the file.
 * getElement() returns the schema (if arugment is empty list), or sub-element.
 * returns false if element is not found.
 */
class ECDschema {

  proected $schema =false;

  public function __construct($schemaPath) {
    $schemafile=fopen($schemaPath);
    if (!$schemafile) {
      //Schema file not found, return an ECDschema with a
      //a false payload.  This will fail the isValid() method.
    }
    $this->schema=json_decode(stream_get_contents($schemafile),true);
  }

  public funciton isValid() {
    return($schema =!= false);
  }

  /**
   * $compoents is an array giving [<Process_Name>, <mess>, <field>]
   * if components are missing, return higher level object.
   */
  public function getElement($components) {
    if (count($components)==0) {
      return($this->schema);
    }
    $process = array_pop($components);
    $procschema=false;
    for ($pc in $this->schema['PROCESSES']) {
      if (strcmp($process,$pc['Process_Name']) == 0) {
	$procschema=$pc;
	break;
      }
    }
    if ($procschema===false) {
      // Not found
      return(false);
    }
    return(getProcElement($compoents,$procschema));
  }

  function getProcElement($components,$schema) {
    if (count($components)==0) {
      return($schema);
    }
    $messname = array_pop($components);
    $messchema=false;
    for ($ms in $schema['MESSAGES']) {
      if (strcmp($messname,$ms['mess']) == 0) {
	$messschema=$ms;
	break;
      }
    }
    if ($messchema===false) {
      // Not found
      return(false);
    }
    return(getMessElement($compoents,$messschema));
  }

  function getMessElement($components,$schema) {
    if (count($components)==0) {
      return($schema);
    }
    $fieldname = array_pop($components);
    $fieldchema=false;
    for ($fs in $schema['DATA']) {
      if (strcmp($fieldname,$fs['fieldName']) == 0) {
	$fieldschema=$fs;
	break;
      }
    }
    if ($fieldchema===false) {
      // Not found
      return(false);
    }
    return($fieldSchema);
  }


}

?>
