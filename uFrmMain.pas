unit uFrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.ToolWin, Vcl.Menus, Vcl.Samples.Gauges,
  Vcl.Buttons, Vcl.ExtDlgs,
  // my uses
  uFrmAbout, uCombolist, uProxylist, uCloserThread, uBruteforcer,
  JvComponentBase, JvCpuUsage;

type
  TMainForm = class(TForm)
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    LblCombos: TLabel;
    LblProxylist: TLabel;
    LblTested: TLabel;
    LblBad: TLabel;
    LblHit: TLabel;
    LblFree: TLabel;
    LblBan: TLabel;
    CmdFile: TToolButton;
    CmdAbout: TToolButton;
    PopupMenu1: TPopupMenu;
    About1: TMenuItem;
    PopupMenu2: TPopupMenu;
    LoadCombolist1: TMenuItem;
    LoadProxylist1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Gauge: TGauge;
    SpnComboPos: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    SpnThreads: TSpinEdit;
    SpnTimeout: TSpinEdit;
    Label11: TLabel;
    CbxProxyType: TComboBox;
    Label1: TLabel;
    cbxLoadFromAPI: TCheckBox;
    edtApiUrl: TEdit;
    Label12: TLabel;
    spnInterval: TSpinEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Bevel1: TBevel;
    Label16: TLabel;
    LblProxyActive: TLabel;
    Label18: TLabel;
    LblProxyBanned: TLabel;
    Label20: TLabel;
    LblProxyDisabled: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    lblCPM: TLabel;
    Label24: TLabel;
    LblActiveThreads: TLabel;
    cmdStart: TSpeedButton;
    cmdStop: TSpeedButton;
    cmdCombo: TSpeedButton;
    cmdProxy: TSpeedButton;
    OpenTextFileDialog1: TOpenTextFileDialog;
    Timer: TTimer;
    Timer1: TTimer;
    JvCpuUsage: TJvCpuUsage;
    lbl2fa: TLabel;
    Label19: TLabel;
    Label17: TLabel;
    LblUnkown: TLabel;
    procedure CmdAboutClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure CmdFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdComboClick(Sender: TObject);
    procedure cmdProxyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdStartClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure cmdStopClick(Sender: TObject);
    procedure cbxLoadFromAPIClick(Sender: TObject);
  private
    { Private declarations }
    AboutBox: TAboutBox;
    FComboList: TCombolist;
    FProxyList: TProxylist;
    FCloserThread: TCloserThread;
    FBruteforcer: TBruteforceEngine;
    procedure BruteforceUpdateStatistics(Sender: TObject);
    procedure BruteforceEngineStart(Sender: TObject);
    procedure BruteforceEngineStop(Sender: TObject);
    procedure EnableCtrls(Enable: Boolean);
    function SanityCheck: Boolean;
    procedure CleanUp;
    procedure InitEnv;
    procedure ExceptionHandler(Sender: TObject; E: Exception);
  public
    { Public declarations }
    StrLocPath: string;
    property Bruteforcer: TBruteforceEngine read FBruteforcer
      write FBruteforcer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure DisplayErrMessage(const Msg: string);
begin
  MessageBox(Application.Handle, @Msg[1], 'Application', MB_OK + MB_ICONERROR);
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  if Assigned(AboutBox) = False then
    AboutBox := TAboutBox.Create(Self);
  AboutBox.Show;
end;

procedure TMainForm.BruteforceEngineStart(Sender: TObject);
begin
  Timer.OnTimer := BruteforceUpdateStatistics;
  EnableCtrls(False);
end;

procedure TMainForm.BruteforceEngineStop(Sender: TObject);
begin
  BruteforceUpdateStatistics(Sender);
  EnableCtrls(True);
  Timer.OnTimer := nil;
end;

procedure TMainForm.BruteforceUpdateStatistics(Sender: TObject);
var
  Engine: TBruteforceEngine;
begin
  Engine := FBruteforcer as TBruteforceEngine;
  with Engine.Statistics^ do
  begin
    LblTested.Caption := IntToStr(Tested);
    LblBad.Caption := IntToStr(Bad);
    LblHit.Caption := IntToStr(Hit);
    LblFree.Caption := IntToStr(Free);
    LblBan.Caption := IntToStr(Ban);
    lblCPM.Caption := IntToStr(CPM);
    lbl2fa.Caption := IntToStr(TwoFa);
    LblUnkown.Caption := IntToStr(Uknown);
    LblActiveThreads.Caption := IntToStr(Engine.ThreadCount);
    LblProxyActive.Caption := IntToStr(Engine.ProxyList.Active);
    LblProxyBanned.Caption := IntToStr(Engine.ProxyList.Banned);
    Gauge.Progress := Engine.Settings.Position;
    StatusBar.Panels[1].Text := IntToStr(Engine.Settings.Position) + '/' +
      IntToStr(Engine.ComboList.Count) + ' (' +
      IntToStr(Gauge.PercentDone) + '%)';
    SpnComboPos.Value := Engine.Settings.Position;
    if Engine.UpdateFromAPI then
      LblProxylist.Caption := IntToStr(Engine.ProxyList.Count);
  end;
end;

procedure TMainForm.cbxLoadFromAPIClick(Sender: TObject);
begin
  edtApiUrl.Enabled := cbxLoadFromAPI.Checked;
  spnInterval.Enabled := cbxLoadFromAPI.Checked;
  Label12.Enabled := cbxLoadFromAPI.Checked;
  Label13.Enabled := cbxLoadFromAPI.Checked;
end;

procedure TMainForm.CleanUp;
begin
  if Assigned(FComboList) then
  begin
    FComboList.Clear;
    FreeAndNil(FComboList);
  end;
  if Assigned(FProxyList) then
  begin
    FProxyList.Clear;
    FreeAndNil(FProxyList);
  end;
  if Assigned(FBruteforcer) then
    FreeAndNil(FBruteforcer);
  if Assigned(AboutBox) then
    FreeAndNil(AboutBox);
  // if Assigned(FCloserThread) then
  // FreeAndNil(FCloserThread);
end;

procedure TMainForm.CmdAboutClick(Sender: TObject);
var
  Point: TPoint;
begin
  GetCursorPos(Point);
  PopupMenu1.Popup(Point.X, Point.Y);
end;

procedure TMainForm.cmdComboClick(Sender: TObject);
begin
  if Assigned(FComboList) then
    FComboList.Clear
  else
    FComboList := TCombolist.Create(True);
  with OpenTextFileDialog1 do
  begin
    if Execute then
    begin
      FComboList.LoadFromFile(FileName);
      StatusBar.Panels[0].Text := 'File: ' + ExtractFileName(FileName);
      LblCombos.Caption := IntToStr(FComboList.Count);
      SpnComboPos.MaxValue := FComboList.Count;
      Gauge.MaxValue := FComboList.Count;
      Gauge.Progress := 0;
    end;
  end;
end;

procedure TMainForm.CmdFileClick(Sender: TObject);
var
  Point: TPoint;
begin
  GetCursorPos(Point);
  PopupMenu2.Popup(Point.X, Point.Y);
end;

procedure TMainForm.cmdProxyClick(Sender: TObject);
begin
  if Assigned(FProxyList) then
    FProxyList.Clear
  else
    FProxyList := TProxylist.Create(True);

  with OpenTextFileDialog1 do
  begin
    if Execute then
    begin
      FProxyList.LoadFromFile(FileName);
      LblProxylist.Caption := IntToStr(FProxyList.Count);
    end;
  end;
end;

procedure TMainForm.cmdStartClick(Sender: TObject);
begin
  if SanityCheck = False then
    Exit;
  if Assigned(FBruteforcer) = False then
    FBruteforcer := TBruteforceEngine.Create;
  if Assigned(FProxyList) = False then
    FProxyList := TProxylist.Create;
  FBruteforcer.ThreadMax := SpnThreads.Value;
  FBruteforcer.ComboList := FComboList;
  FBruteforcer.ProxyList := FProxyList;
  FBruteforcer.ProxyType := CbxProxyType.ItemIndex;
  FBruteforcer.Timeout := SpnTimeout.Value;
  FBruteforcer.Position := SpnComboPos.Value;
  FBruteforcer.UpdateFromAPI := cbxLoadFromAPI.Checked;
  FBruteforcer.ApiUrl := edtApiUrl.Text;
  FBruteforcer.Interval := spnInterval.Value * 60000;
  FBruteforcer.OnEngineStart := BruteforceEngineStart;
  FBruteforcer.OnEngineComplete := BruteforceEngineStop;
  FBruteforcer.OnUpdateStatistics := nil;
  FBruteforcer.InitializeEngine;
  FBruteforcer.StartBruteforce;
end;

procedure TMainForm.cmdStopClick(Sender: TObject);
begin
  FBruteforcer.StopBruteforce;
end;

procedure TMainForm.EnableCtrls(Enable: Boolean);
begin
  cmdStart.Enabled := Enable;
  cmdStop.Enabled := not Enable;
  cmdCombo.Enabled := Enable;
  cmdProxy.Enabled := Enable;
  LoadCombolist1.Enabled := Enable;
  LoadProxylist1.Enabled := Enable;
  SpnThreads.Enabled := Enable;
  SpnTimeout.Enabled := Enable;
  SpnComboPos.Enabled := Enable;
  spnInterval.Enabled := Enable;
  edtApiUrl.Enabled := Enable;
  CbxProxyType.Enabled := Enable;
  cbxLoadFromAPI.Enabled := Enable;
  Timer.Enabled := not Enable;
end;

procedure TMainForm.ExceptionHandler(Sender: TObject; E: Exception);
var
  S: string;
  Name: string;
begin
  // stop egine if running
  if Assigned(FBruteforcer) and (FBruteforcer.ThreadCount <> 0) then
  begin
    FBruteforcer.StopBruteforce;
    FBruteforcer.HardAbort;
  end;
  S := 'Thrown by: ' + Sender.ClassName + #10;
  S := S + 'Message: ' + E.Message + #10;
  Name := E.ClassName;
  MessageBox(Application.Handle, @S[1], @Name[1], MB_OK + MB_ICONERROR);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CleanUp;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  if Assigned(FCloserThread) then
    Exit;

  if MessageBox(Application.Handle, 'Do you really want to close me ?',
    'Applicatioin', MB_YESNO + MB_ICONINFORMATION) = mrYes then
  begin
    if (Assigned(FBruteforcer) = False) or (FBruteforcer.ThreadCount = 0) then
      CanClose := True
    else
      FCloserThread := TCloserThread.Create(False);
  end;

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := ExceptionHandler;
  StrLocPath := ExtractFilePath(Application.ExeName);
  InitEnv;
end;

procedure TMainForm.InitEnv;
begin
  if DirectoryExists(StrLocPath + 'Result\') = False then
    CreateDir(StrLocPath + 'Result\');
end;

function TMainForm.SanityCheck: Boolean;
begin
  Result := False;
  if (Assigned(FComboList) = False) or (FComboList.Count = 0) then
  begin
    DisplayErrMessage('Please Load combo list');
    Exit;
  end;
  if cbxLoadFromAPI.Checked and (edtApiUrl.Text = '') then
  begin
    DisplayErrMessage('Please set API url to update proxies from!');
    Exit;
  end;
  if (cbxLoadFromAPI.Checked = False) and
    ((Assigned(FProxyList) = False) or (FProxyList.Count = 0)) then
  begin
    DisplayErrMessage('Please Load proxy List');
    Exit;
  end;
  Result := True;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  StatusBar.Panels[2].Text := Format('CPU: %.2F%%', [JvCpuUsage.Usage]);
end;

end.
