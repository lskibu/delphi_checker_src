unit uFunctions;

interface

uses Classes, SysUtils, DateUtils;

procedure WriteHitToTextFile(const FileName, Username, Password, Proxy: string;
  Capture: TStringList); overload;

procedure WriteLintToTextFile(const FileName, Line: string;
  _Append: boolean = False);

function DateTimeAsString(Date: TDateTime): string;

function DetectFileEncoding(const FileName: string): TEncoding;

implementation

procedure WriteHitToTextFile(const FileName, Username, Password, Proxy: string;
  Capture: TStringList);

var
  sw: TStreamWriter;
  S: string;
begin
  sw := TStreamWriter.Create(FileName, True, TEncoding.Unicode);
  try
    sw.WriteLine('++++++++++++++++++++++++++++++++++++++++ Uplay' +
      ' Checker ++++++++++++++++++++++++++++++++++++');
    sw.WriteLine('[+] Combo: ' + Username + ':' + Password);
    sw.WriteLine('[+] User: ' + Username);
    sw.WriteLine('[+] Password: ' + Password);
    sw.WriteLine('[+] Proxy: ' + Proxy);
    sw.WriteLine('[+] Capture:');
    for S in Capture do
      sw.WriteLine(S);
  finally
    sw.Close;
    sw.Free;
  end;
end;

procedure WriteLintToTextFile(const FileName, Line: string;
  _Append: boolean = False);
var
  sw: TStreamWriter;
begin
  sw := TStreamWriter.Create(FileName, _Append_, TEncoding.UTF8);
  try
    sw.WriteLine(Line);
  finally
    sw.Close;
    sw.Free;
  end;
end;

function DateTimeAsString(Date: TDateTime): string;
var
  fmt: TFormatSettings;
begin
  fmt := TFormatSettings.Create();
  fmt.DateSeparator := '-';
  fmt.TimeSeparator := '.';
  Result := DateTimeToStr(Date, fmt);
end;

function DetectFileEncoding(const FileName: string): TEncoding;
var
  Len: integer;
  Buf: TBytes;
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Buf, 4);
    Len := fs.Read(Buf, 4);
    if Len < 4 then
      SetLength(Buf, Len);
    Result := nil;
    TEncoding.GetBufferEncoding(Buf, Result);
  finally
    fs.Free;
    SetLength(Buf, 0);
  end;;
end;

end.
