(*------------------------------------------------------------------------------
Arakis

This isn't actually an executable application. This DPR is provided as a
convenient way to recompile the engine, since you'll need DCUs or other apps
use'ing these engine files will complain.

It's named Arakis because, if you couldn't tell by now, nearly all of the Leto
project takes names liberally from Frank Herbert's "Dune" series, which the
author has a particular fondness for. "Arakis" is a suitable pseudonym for
"Engine" if you want to sound techy when referring to Leto's engine: "You'll
need to download and compile Arakis if you want to mod NWNX-Leto," much like
Windows geeks will say "Chicago", "Memphis", "Cairo", and so on.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

program Arakis;

{$APPTYPE CONSOLE}

{%ToDo 'Arakis.todo'}

uses
  SysUtils,
  Header_Leto in 'Header_Leto.pas',
  Class_GffField in 'Class_GffField.pas',
  Class_GffFile in 'Class_GffFile.pas',
  Class_ErfStruct in 'Class_ErfStruct.pas',
  Class_ErfFile in 'Class_ErfFile.pas',
  Class_LetoFile in 'Class_LetoFile.pas',
  Class_TlkFile in 'Class_TlkFile.pas',
  Class_XbaseFile in 'Class_XbaseFile.pas',
  Class_LetoXml in 'Class_LetoXml.pas',
  Class_MultiEvent in 'Class_MultiEvent.pas',
  Class_2daFile in 'Class_2daFile.pas';

begin

end.
 