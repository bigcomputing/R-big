Option Explicit

Dim args, arg, icmd, i
Dim host, user, passwd, verbose, simplequote
Dim locator, server, wmiservice, processSU, config
Dim cmd, ret

' Process the arguments
Set args = WScript.Arguments

' Set default option values
verbose = False
simplequote = False
user = ""
passwd = ""

If args.Count = 0 Then
  usage()
End If

host = args.Item(0)
If host = "-h" Then usage()

i = 1
icmd = 0
Do While i < args.Count
  arg = args.Item(i)
  If Left(arg, 1) = "-" Then
    Select Case arg
      Case "-h"
        usage()
      Case "-v"
        verbose = True
        WScript.Echo("Setting verbose option")
      Case "-n"
        ' Ignore this option
        If verbose Then WScript.Echo("Ignoring -n option")
      Case "-l"
        i = i + 1
        If i >= args.Count Then
          WScript.Echo("The -l options takes a required argument")
          usage()
        End If
        user = args.Item(i)
        If verbose Then WScript.Echo("User name: " & user)
      Case "-p"
        i = i + 1
        If i >= args.Count Then
          WScript.Echo("The -p options takes a required argument")
          usage()
        End If
        passwd = args.Item(i)
        If verbose Then WScript.Echo("Passwd: " & passwd)
      Case "-P"
        ' XXX Some way to avoid echoing?
        passwd = InputBox("Please enter password", "Password Entry")
        If verbose Then WScript.Echo("Passwd: " & passwd)
      Case "-s"
        simplequote = True
        If verbose Then WScript.Echo("Using simple quoting")
      Case "--"
        icmd = i + 1
        Exit Do
      Case Else
        WScript.Echo("Illegal option specified: " & arg)
        usage()
    End Select
    i = i + 1
  Else
    icmd = i
    Exit Do
  End If
Loop

If icmd = 0 Or icmd >= args.Count Then
  WScript.Echo("No command specified")
  usage()
End If

' Cannot specified alternate user without a password
If passwd = "" Then
  user = ""
End If

' Cannot use alternative credentials on local machine
If isLocal(host) Then
  user = ""
  passwd = ""
  If verbose Then WScript.Echo("Local target computer detected")
End If

If verbose Then
  If passwd = "" Then
    WScript.Echo("Not using alternative credentials")
  Else
    WScript.Echo("Using alternative credentials")
  End If
End If

Set locator = CreateObject("WbemScripting.SWbemLocator")
Set server = locator.ConnectServer(host, "root\cimv2", user, passwd, "", "")
Set wmiservice = server.Get("Win32_Process")
Set processSU = server.Get("win32_ProcessStartUP")
Set config = processSU.SpawnInstance_
config.ShowWindow = 0  ' Hide the window

' Create the command to execute on the target computer
If simplequote Then
  cmd = simple_quote(args.Item(icmd))
  For i = icmd + 1 To args.Count - 1
    cmd = cmd & " " & simple_quote(args.Item(i))
  Next
Else
  cmd = msc_quote(args.Item(icmd))
  For i = icmd + 1 To args.Count - 1
    cmd = cmd & " " & msc_quote(args.Item(i))
  Next
End If

' Execute the command on the target computer asynchronously
If verbose Then WScript.Echo("Executing remote command: " & cmd)
ret = wmiservice.create(cmd, Null, config)

' Check for errors
If ret <> 0 Then
  error("Error creating remote process: " & ret)
End If

Function error(msg)
  WScript.Echo(msg)
  WScript.Quit(1)
End Function

Function usage()
  error("Usage: rwin <host> [<option> ...] [--] <cmd> [<arg> ...]" & vbCrLf _
        & " where <option> can be:"                                & vbCrLf _
        & "   -v              enables verbose mode"                & vbCrLf _
        & "   -l <username>   specifies remote user"               & vbCrLf _
        & "   -p <password>   specifies remote password"           & vbCrLf _
        & "   -P              prompts for remote password"         & vbCrLf _
        & "   -s              enables simple quoting"              & vbCrLf _
        & "   -h              prints this usage message"           & vbCrLf _
        & "   -n              ignored (for rsh compatibility)")
End Function

Function isLocal(ByVal host)
  Dim net

  If host = "." Or StrComp(host, "localhost", vbTextCompare) = 0 Then
    isLocal = True
  Else
    Set net = CreateObject("WScript.Network")
    If StrComp(host, net.ComputerName, vbTextCompare) = 0 Then
      isLocal = True
    Else
      isLocal = False
    End If
  End If
End Function

Function msc_quote(ByVal cmd)
  Dim i, j, c, q, nbs, needq

  q = Chr(34)
  nbs = 0
  needq = False

  For i = 1 To Len(cmd)
    c = Mid(cmd, i, 1)
    If c = "\" Then
      q = q & c
      nbs = nbs + 1
    ElseIf c = Chr(34) Then
      For j = 1 To nbs + 1
        q = q & "\"
      Next
      q = q & c
      nbs = 0
      ' Double quotes need to be quoted
      needq = True
    Else
      q = q & c
      nbs = 0
      ' Whitespace needs to be quoted
      If c = " " Or c = Chr(9) Or c = Chr(10) Or c = Chr(13) Then
        needq = True
      End IF
    End IF
  Next

  For j = 1 To nbs
    q = q & "\"
  Next

  q = q & Chr(34)

  ' Quote if zero length, or contains either
  ' double quote or whitespace
  If needq Or Len(cmd) = 0 Then
    msc_quote = q
  Else
    msc_quote = cmd
  End If
End Function

Function simple_quote(ByVal cmd)
  Dim i, c, needq

  needq = False
  For i = 1 To Len(cmd)
    c = Mid(cmd, i, 1)
    If c = " " Or c = Chr(9) Or c = Chr(10) Or c = Chr(13) Then
      needq = True
    End If

    ' I do not think this can happen (oddly enough)
    If c = Chr(34) Then
      error("Illegal double quote used in command argument")
    End If
  Next

  If needq Or Len(cmd) = 0 Then
    simple_quote = Chr(34) & cmd & Chr(34)
  Else
    simple_quote = cmd
  End If
End Function
