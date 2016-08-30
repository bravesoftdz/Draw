{
  Draw - An Application for Drawing and Image Manipulation
  Copyright (C) 2013-2016  René Hickersberger

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  FITNESS FOR A PARTICULAR PURPOSE.
}

program Draw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lz_printers, pl_bgracontrols, pl_exdesign, pl_astronomy,
  pl_bgrauecontrols, pl_excontrols, pl_exsystem, lz_fpweb, pl_freespider,
  pl_htmlviewer, pl_openwire, pl_powerpdf, Unit1, Unit2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormProperties, FormProperties);
  Application.Run;
end.

