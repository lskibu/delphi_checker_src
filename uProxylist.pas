unit uProxylist;

interface

uses Classes, Types, uTypes;

type
  // proxy class
  TProxy = class
  protected
    FHost: string;
    FPort: string;
    FUsername: string;
    FPassword: string;
    FActive: boolean;
  public
    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property Active: boolean read FActive write FActive;
    function WriteToString: string;
    procedure ReadFromStr(const S: string);
  end;

  // proxylist class
  TProxyList = class(TList)
  protected
    FOwnsObjects: boolean;
    FBanned: integer;
    function FindDuplicates: integer;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetActive: integer;
    function GetItem(index: integer): TProxy;
    procedure SetItem(index: integer; Item: TProxy);
    procedure Initialize;
  public
    constructor Create(OwnsObjects: boolean); overload;
    constructor Create(const FileName: string); overload;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStrings(StrList: TStringList);
    procedure SaveToFile(const FileName: string);
    function RemoveDuplicates: integer;
    procedure BanProxy(const index: integer);
    procedure Activate(const index: integer);
    procedure ActivateAll;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    property Active: integer read GetActive;
    property Banned: integer read FBanned;
    property Items[index: integer]: TProxy read GetItem write SetItem; default;
  end;

implementation

uses SysUtils, StrUtils, uFunctions;

function ProxyComparer(Ptr1, Ptr2: Pointer): integer;
begin
  if (TProxy(Ptr1).WriteToString < TProxy(Ptr2).WriteToString) then
    Result := -1
  else if (TProxy(Ptr1).WriteToString > TProxy(Ptr2).WriteToString) then
    Result := 1
  else
    Result := 0;
end;

{ TProxylist }

procedure TProxyList.Activate(const index: integer);
begin
  if Items[index].Active = False then
  begin
    Items[index].Active := True;
    Dec(FBanned);
  end;
end;

procedure TProxyList.ActivateAll;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Activate(I);
  Initialize;
end;

procedure TProxyList.BanProxy(const index: integer);
begin
  if Items[index].Active then
  begin
    Items[index].Active := False;
    Inc(FBanned);
  end;
end;

constructor TProxyList.Create(OwnsObjects: boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
end;

constructor TProxyList.Create(const FileName: string);
begin
  Create(True);
  LoadFromFile(FileName);
  Initialize;
end;

function TProxyList.FindDuplicates: integer;
var
  I: integer;
begin
  Result := 0;
  for I := Count - 1 downto 1 do
    if Items[I].WriteToString = Items[I - 1].WriteToString then
    begin
      Delete(I);
      Inc(Result);
    end;
  Initialize;
end;

function TProxyList.GetActive: integer;
begin
  Result := Count - FBanned;
end;

function TProxyList.GetItem(index: integer): TProxy;
begin
  Result := inherited Items[index];
end;

procedure TProxyList.Initialize;
var
  I: integer;
begin
  FBanned := 0;
  for I := 0 to Count - 1 do
    if Items[I].Active = False then
      Inc(FBanned);
end;

procedure TProxyList.LoadFromFile(const FileName: string);
var
  StreamReader: TStreamReader;
  S: string;
  Proxy: TProxy;
  Enc: TEncoding;
begin
  Enc := DetectFileEncoding(FileName);
  StreamReader := TStreamReader.Create(FileName, Enc);
  try
    while not StreamReader.EndOfStream do
    begin
      S := Trim(StreamReader.ReadLine);
      if S <> '' then
      begin
        Proxy := TProxy.Create;
        // Proxy.Active := True;
        Proxy.ReadFromStr(S);
        if Proxy.Host <> '' then
          Add(Proxy)
        else
          Proxy.Free;
      end;
    end;
  finally
    StreamReader.Close;
    StreamReader.Free;
  end;
end;

procedure TProxyList.LoadFromStrings(StrList: TStringList);
var
  S: string;
  Proxy: TProxy;
begin
  for S in StrList do
  begin
    Proxy := TProxy.Create;
    Proxy.ReadFromStr(Trim(S));
    if Proxy.Host <> '' then
      Add(Proxy)
    else
      Proxy.Free;
  end;

end;

procedure TProxyList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FOwnsObjects then
    if Action = lnDeleted then
      TProxy(Ptr).Free;
  inherited Notify(Ptr, Action);
end;

function TProxyList.RemoveDuplicates: integer;
begin
  Sort(ProxyComparer);
  Result := FindDuplicates;
  Initialize;
end;

procedure TProxyList.SaveToFile(const FileName: string);
var
  StreamWriter: TStreamWriter;
  Proxy: TProxy;
  I: integer;
begin
  StreamWriter := TStreamWriter.Create(FileName);
  try
    for I := 0 to Count - 1 do
      StreamWriter.WriteLine(Items[I].WriteToString);
  finally
    StreamWriter.Close;
    StreamWriter.Free;
  end;
end;

procedure TProxyList.SetItem(index: integer; Item: TProxy);
begin
  inherited Items[index] := Item;
end;

{ TProxy }

procedure TProxy.ReadFromStr(const S: string);
var
  StrArray: TStringDynArray;
  Len: integer;
begin
  StrArray := SplitString(S, ':');
  Len := Length(StrArray);
  if Len >= 2 then
  begin
    FHost := StrArray[0];
    FPort := StrArray[1];
  end;
  if Len = 4 then
  begin
    FUsername := StrArray[2];
    FPassword := StrArray[3];
  end;
  FActive := True;
end;

function TProxy.WriteToString: string;
begin
  Result := FHost + ':' + FPort;
  if FUsername <> '' then
    Result := Result + ':' + FUsername + ':' + FPassword;
end;

end.
