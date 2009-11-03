$DR = new-object -com DelphiRemoteServer.Service

function Invoke-IDispatchMethod([string]$MethodName=$null, [System.__ComObject]$DispIntf=$null, $Params=$null)
{
  Begin {
    # Executes once before first item in pipeline is processed
    if ($DispIntf) {
      $DispIntf.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::InvokeMethod, $null, $DispIntf, $Params);
      break;
    }
  }

  Process {
    # Executes once for each pipeline object
    if($_) {
      $_.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::InvokeMethod, $null, $_, $Params)
    }
  }

  End {
    # Executes once after last pipeline object is processed
  }
}

function Invoke-IDispatchPropertyGet([string]$MethodName=$null, [System.__ComObject]$DispIntf=$null, $Params=$null)
{
  Begin {
    # Executes once before first item in pipeline is processed
    if ($DispIntf) {
      $DispIntf.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::GetProperty, $null, $DispIntf, $Params);
      break;
    }
  }

  Process {
    # Executes once for each pipeline object
    if($_) {
      $_.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::GetProperty, $null, $_, $Params)
    }
  }

  End {
    # Executes once after last pipeline object is processed
  }
}

function Invoke-IDispatchPropertySet([string]$MethodName=$null, [System.__ComObject]$DispIntf=$null, $Params=$null)
{
  Begin {
    # Executes once before first item in pipeline is processed
    if ($DispIntf) {
      $DispIntf.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::SetProperty, $null, $DispIntf, $Params);
      break;
    }
  }

  Process {
    # Executes once for each pipeline object
    if($_) {
      $_.psbase.GetType().InvokeMember($MethodName,[Reflection.BindingFlags]::SetProperty, $null, $_, $Params)
    }
  }

  End {
    # Executes once after last pipeline object is processed
  }
}

function DelphiRemote-GetChild([string]$ChildName=$null, [System.__ComObject]$DispIntf=$null)
{
  Begin {
    # Executes once before first item in pipeline is processed
    if ($DispIntf) {
      Invoke-IDispatchMethod GetChild $DipsIntf $ChildName;
      break;
    }
  }

  Process {
    # Executes once for each pipeline object
    if($_) {
      Invoke-IDispatchMethod GetChild $_ $ChildName;
    }
  }

  End {
    # Executes once after last pipeline object is processed
  }
}