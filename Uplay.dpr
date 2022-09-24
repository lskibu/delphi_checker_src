program Uplay;

uses
  Vcl.Forms,
  uFrmMain in 'uFrmMain.pas' {Form1} ,
  uFrmAbout in 'uFrmAbout.pas' {AboutBox} ,
  uProxylist in 'uProxylist.pas',
  uTypes in 'uTypes.pas',
  uCombolist in 'uCombolist.pas',
  uThread in 'uThread.pas',
  uFunctions in 'uFunctions.pas',
  uBruteforcer in 'uBruteforcer.pas',
  uFrmWaiter in 'uFrmWaiter.pas' {FrmWaiter} ,
  uCloserThread in 'uCloserThread.pas',
  uUplayBruteProc in 'uUplayBruteProc.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
