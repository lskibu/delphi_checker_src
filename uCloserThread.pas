unit uCloserThread;

interface

uses
  System.Classes, uFrmWaiter;

type
  TCloserThread = class(TThread)
  private
    { Private declarations }
    FrmWaiter: TFrmWaiter;
    procedure ShowWaitFrm;
    procedure CloseMainFrm;
  protected
    procedure Execute; override;
  end;

implementation

uses uFrmMain, Windows;

{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

  Synchronize(UpdateCaption);

  and UpdateCaption could look like,

  procedure TCloserThread.UpdateCaption;
  begin
  Form1.Caption := 'Updated in a thread';
  end;

  or

  Synchronize(
  procedure
  begin
  Form1.Caption := 'Updated in thread via an anonymous method'
  end
  )
  );

  where an anonymous method is passed.

  Similarly, the developer can call the Queue method with similar parameters as
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.

}

{ TCloserThread }

procedure TCloserThread.CloseMainFrm;
begin
  Synchronize(MainForm.Close);
end;

procedure TCloserThread.Execute;
begin
  { Place thread code here }
  FreeOnTerminate := True;
  Synchronize(ShowWaitFrm);
  MainForm.Bruteforcer.StopBruteforce;
  while not MainForm.Bruteforcer.Stopped do
    Sleep(500);
  MainForm.OnCloseQuery := nil;
  Synchronize(FrmWaiter.Close);
  CloseMainFrm;
end;

procedure TCloserThread.ShowWaitFrm;
begin
  FrmWaiter := TFrmWaiter.Create(nil);
  FrmWaiter.Show;
  FrmWaiter.Update;
end;

end.
