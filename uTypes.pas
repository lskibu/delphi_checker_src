unit uTypes;

interface

uses Classes, ipwhttp, windows;

type

  // Proxy protocol enumeration
  TProxyProtocol = (PtHttp, PtSocks4, PtSocks5);
  // Bot status
  TStatus = (stHit, stBad, stBan, stFree, st2fa, stRetry, stUnkown);

  // statistics struct (record)
  TStatistics = record
    Tested: integer;
    Bad: integer;
    Hit: integer;
    Free: integer;
    Ban: integer;
    Retry: integer;
    Uknown: integer;
    TwoFa: integer;
    CPM: integer;
  end;

  PStatistics = ^TStatistics;

  TBruteforceSettings = record
    Position: integer;
    ProxyPosition: integer;
    ResultDir: string;
    StartTime: Int64;
    LockCombo: RTL_CRITICAL_SECTION;
    LockProxy: RTL_CRITICAL_SECTION;
    LockCheck: RTL_CRITICAL_SECTION;
    LockStatus: RTL_CRITICAL_SECTION;
  end;

  PBruteforceSettings = ^TBruteforceSettings;

  // Bot thread procedure
  TBotThreadProc = procedure(Http: TipwHTTP; const Username, Password: string;
    var Status: TStatus; Capture: TStringList);

  TProcUpdateListView = procedure(Sender: TObject;
    const Username, Password: string; const Proxy: string; const Status: string)
    of object;

implementation

end.
