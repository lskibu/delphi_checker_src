unit uCombolist;

interface

uses Classes, SysUtils;

type
  TCombo = class
  protected
    FUsername: string;
    FPassword: string;
  public
    procedure ReadFromString(const S: string);
    function WriteToString: string;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;

  TComboList = class(TList)
  protected
    FOwnsObjects: boolean;
    FFileName: string;
    FEncoding: TEncoding;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure SetItem(index: Integer; Combo: TCombo);
    function GetItem(index: Integer): TCombo;
    function FindDuplicates: Integer;
  public
    constructor Create(OwnsObjects: boolean); overload;
    constructor Create(const FileName: string); overload;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function RemoveDuplicates: Integer;
    procedure Clear; override;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects;
    property FileName: string read FFileName write FFileName;
    property Encoding: TEncoding read FEncoding write FEncoding;
    property Items[Index: Integer]: TCombo read GetItem write SetItem; default;
  end;

implementation

uses uFunctions;

function ComboComparer(Ptr1, Ptr2: Pointer): Integer;
begin
  if TCombo(Ptr1).WriteToString > TCombo(Ptr2).WriteToString then
    Result := 1
  else if TCombo(Ptr1).WriteToString < TCombo(Ptr2).WriteToString then
    Result := -1
  else
    Result := 0;
end;

{ TCombo }

procedure TCombo.ReadFromString(const S: string);
var
  P: Integer;
begin
  P := Pos(':', S);
  if P = 0 then
    Exit;
  FUsername := Copy(S, 1, P - 1);
  FPassword := Copy(S, P + 1, Length(S));
end;

function TCombo.WriteToString: string;
begin
  Result := FUsername + ':' + FPassword;
end;

{ TComboList }

procedure TComboList.Clear;
begin
  FFileName := '';
  inherited Clear;
end;

constructor TComboList.Create(OwnsObjects: boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
  FEncoding := nil;
end;

constructor TComboList.Create(const FileName: string);
begin
  Create(True);
  LoadFromFile(FileName);
end;

function TComboList.FindDuplicates: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 1 do
    if Items[I].WriteToString = Items[I - 1].WriteToString then
    begin
      Delete(I);
      Inc(Result);
    end;
end;

function TComboList.GetItem(index: Integer): TCombo;
begin
  Result := inherited Items[Index];
end;

procedure TComboList.LoadFromFile(const FileName: string);
var
  StreamReader: TStreamReader;
  S: string;
  Combo: TCombo;
begin
  FEncoding := DetectFileEncoding(FileName);
  StreamReader := TStreamReader.Create(FileName, FEncoding);
  try
    while not StreamReader.EndOfStream do
    begin
      S := Trim(StreamReader.ReadLine);
      Combo := TCombo.Create;
      Combo.ReadFromString(S);
      if Combo.Username <> '' then
        Add(Combo)
      else
        Combo.Free;
    end;
  finally
    StreamReader.Close;
    StreamReader.Free;
    FFileName := FileName;
  end;
end;

procedure TComboList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if FOwnsObjects then
    if Action = lnDeleted then
      TCombo(Ptr).Free;
  inherited Notify(Ptr, Action);
end;

function TComboList.RemoveDuplicates: Integer;
begin
  Sort(ComboComparer);
  Result := FindDuplicates;
end;

procedure TComboList.SaveToFile(const FileName: string);
var
  StreamWriter: TStreamWriter;
  I: Integer;
begin
  StreamWriter := TStreamWriter.Create(FileName, False, FEncoding);
  try
    for I := 0 to Count - 1 do
      StreamWriter.WriteLine(Items[I].WriteToString);
  finally
    StreamWriter.Close;
    StreamWriter.Free;
  end;
end;

procedure TComboList.SetItem(index: Integer; Combo: TCombo);
begin
  inherited Items[Index] := Combo;
end;

end.
