unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnCommon, CnClasses, CnTimer, ComCtrls, CnWinampCtrl;

type

  TTrackBarHacker = class(TTrackBar);

  TForm1 = class(TForm)
    btnShufleOn: TButton;
    btnShufleOff: TButton;
    btnGetPlayList: TButton;
    lst1: TListBox;
    lbl1: TLabel;
    btnFindWinamp: TButton;
    btnPrevTrack: TButton;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    btnNextTrack: TButton;
    lblTime: TLabel;
    btnGetVersion: TButton;
    btnRepeatOn: TButton;
    btnRepeatOff: TButton;
    btnIsFound: TButton;
    lblLength: TLabel;
    tbTime: TTrackBar;
    btnVolumeUp: TButton;
    btnVolumeDown: TButton;
    tbVolume: TTrackBar;
    lblVolume: TLabel;
    CnTimerList1: TCnTimerList;
    lblTitle: TLabel;
    tbVolBalance: TTrackBar;
    btnStartWinamp: TButton;
    btnCloseWinamp: TButton;
    btnEnabledWAWindow: TButton;
    btnDisabledWAWindow: TButton;
    tbEQ10: TTrackBar;
    tbEQ0: TTrackBar;
    tbEQ1: TTrackBar;
    tbEQ2: TTrackBar;
    tbEQ3: TTrackBar;
    tbEQ4: TTrackBar;
    tbEQ5: TTrackBar;
    tbEQ6: TTrackBar;
    tbEQ7: TTrackBar;
    tbEQ8: TTrackBar;
    tbEQ9: TTrackBar;
    btnEnabledEQ: TButton;
    btnEQAutoLoad: TButton;
    btnAbout: TButton;
    CtrlWinamp: TCnWinampCtrl;
    edtPath: TEdit;
    lblPath: TLabel;
    procedure btnShufleOnClick(Sender: TObject);
    procedure btnShufleOffClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetPlayListClick(Sender: TObject);
    procedure btnFindWinampClick(Sender: TObject);
    procedure btnPrevTrackClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnNextTrackClick(Sender: TObject);
    procedure btnGetVersionClick(Sender: TObject);
    procedure btnRepeatOnClick(Sender: TObject);
    procedure btnRepeatOffClick(Sender: TObject);
    procedure btnIsFoundClick(Sender: TObject);
    procedure btnVolumeUpClick(Sender: TObject);
    procedure btnVolumeDownClick(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure CnTimerList1Items0Timer(Sender: TObject);
    procedure CnTimerList1Items1Timer(Sender: TObject);
    procedure lst1DblClick(Sender: TObject);
    procedure tbVolBalanceChange(Sender: TObject);
    procedure btnStartWinampClick(Sender: TObject);
    procedure btnCloseWinampClick(Sender: TObject);
    procedure tbTimeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnEnabledWAWindowClick(Sender: TObject);
    procedure btnDisabledWAWindowClick(Sender: TObject);
    procedure btnEnabledEQClick(Sender: TObject);
    procedure btnEQAutoLoadClick(Sender: TObject);
    procedure tbEQ10Change(Sender: TObject);
    procedure tbEQ0Change(Sender: TObject);
    procedure tbEQ1Change(Sender: TObject);
    procedure tbEQ2Change(Sender: TObject);
    procedure tbEQ3Change(Sender: TObject);
    procedure tbEQ4Change(Sender: TObject);
    procedure tbEQ5Change(Sender: TObject);
    procedure tbEQ6Change(Sender: TObject);
    procedure tbEQ7Change(Sender: TObject);
    procedure tbEQ9Change(Sender: TObject);
    procedure tbEQ8Change(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);
  private
    PlayListPos: Integer;
//    CtrlWinamp: TCnWinampCtrl;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.btnShufleOnClick(Sender: TObject);
begin
  CtrlWinamp.WAShufle := True;
end;

procedure TForm1.btnShufleOffClick(Sender: TObject);
begin
  CtrlWinamp.WAShufle := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  CtrlWinamp := TCnWinampCtrl.Create(Self);
//  CtrlWinamp.AutoFind := True;
  TTrackBarHacker(tbTime).OnMouseDown := tbTimeMouseDown;
  TTrackBarHacker(tbTime).OnMouseUp := tbTimeMouseUp;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CtrlWinamp.Free;
end;

procedure TForm1.btnGetPlayListClick(Sender: TObject);
begin
  CtrlWinamp.WAPath := edtPath.Text;
  lst1.Items.CommaText := CtrlWinamp.GetPlayList;
//  lst1.ItemIndex := CtrlWinamp.PlayListPos;
end;

procedure TForm1.btnFindWinampClick(Sender: TObject);
begin
  if CtrlWinamp.FindWinamp then
  begin
    //Volume
    tbVolBalance.Position := CtrlWinamp.VolBalance;
    tbVolume.Position := -CtrlWinamp.Volume;

    //EQData
    tbEQ0.Position := CtrlWinamp.EQData[EQ60hz];
    tbEQ1.Position := CtrlWinamp.EQData[EQ170hz];
    tbEQ2.Position := CtrlWinamp.EQData[EQ310hz];
    tbEQ3.Position := CtrlWinamp.EQData[EQ600hz];
    tbEQ4.Position := CtrlWinamp.EQData[EQ1k];
    tbEQ5.Position := CtrlWinamp.EQData[EQ3k];
    tbEQ6.Position := CtrlWinamp.EQData[EQ6k];
    tbEQ7.Position := CtrlWinamp.EQData[EQ12k];
    tbEQ8.Position := CtrlWinamp.EQData[EQ14k];
    tbEQ9.Position := CtrlWinamp.EQData[EQ16k];
    tbEQ10.Position := CtrlWinamp.EQData[EQPreAmp];
    if CtrlWinamp.EQData[EQEnabled] <> 0 then
      btnEnabledEQ.Caption := 'EQ On';
    if CtrlWinamp.EQData[EQAutoLoad] <> 0 then
      btnEQAutoLoad.Caption := 'EQ Load On';

    //PlayList
    btnGetPlayListClick(Sender);

    CnTimerList1.Items[1].Enabled := True;
    CnTimerList1.Items[0].Enabled := True;
    Exit;
  end;
  ShowMessage('Winamp is not found.');
end;

procedure TForm1.btnPrevTrackClick(Sender: TObject);
begin
  CtrlWinamp.PrevTrack;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
begin
  CtrlWinamp.Play;
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
  CtrlWinamp.Pause;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  CtrlWinamp.Stop;
end;

procedure TForm1.btnNextTrackClick(Sender: TObject);
begin
  CtrlWinamp.NextTack;
end;

procedure TForm1.btnGetVersionClick(Sender: TObject);
begin
  ShowMessage(CtrlWinamp.GetVersion);
end;

procedure TForm1.btnRepeatOnClick(Sender: TObject);
begin
  CtrlWinamp.WARepeat := True;
end;

procedure TForm1.btnRepeatOffClick(Sender: TObject);
begin
  CtrlWinamp.WARepeat := False;
end;

procedure TForm1.btnIsFoundClick(Sender: TObject);
begin
  ShowMessage(BoolToStr(CtrlWinamp.IsFound, True));
end;

procedure TForm1.btnVolumeUpClick(Sender: TObject);
begin
  CtrlWinamp.VolumeUp;
end;

procedure TForm1.btnVolumeDownClick(Sender: TObject);
begin
  CtrlWinamp.VolumeDown;
end;

procedure TForm1.tbVolumeChange(Sender: TObject);
begin
  CtrlWinamp.Volume := -tbVolume.Position;
end;

procedure TForm1.CnTimerList1Items0Timer(Sender: TObject);
var
  iTime: Integer;
begin
  iTime := CtrlWinamp.WACurrentTime;
  tbTime.Position := iTime;
  lblTime.Caption := Format('Time: %6.3f', [iTime/1000]);
end;

procedure TForm1.CnTimerList1Items1Timer(Sender: TObject);
begin
  //更新当前播放曲目
  if CtrlWinamp.PlayListPos <> PlayListPos then
  begin
    PlayListPos := CtrlWinamp.PlayListPos;
    lst1.ItemIndex := PlayListPos;
    lblTitle.Caption := lst1.Items[PlayListPos]; 
    tbTime.Max := CtrlWinamp.GetTimeLength * 1000;
    lblLength.Caption := Format('Length: %d Sec', [CtrlWinamp.GetTimeLength]);
  end;
end;

procedure TForm1.lst1DblClick(Sender: TObject);
begin
  if lst1.ItemIndex > -1 then
  begin
    PlayListPos := lst1.ItemIndex;
    CtrlWinamp.PlayListPos := lst1.ItemIndex;
    CtrlWinamp.Play;
  end;
end;

procedure TForm1.tbVolBalanceChange(Sender: TObject);
begin
  CtrlWinamp.VolBalance := tbVolBalance.Position;
end;

procedure TForm1.btnStartWinampClick(Sender: TObject);
begin                   //C:\Program Files\Winamp\Winamp.exe
  CtrlWinamp.StartWinamp('');
end;

procedure TForm1.btnCloseWinampClick(Sender: TObject);
begin
  CtrlWinamp.CloseWinamp;
end;

procedure TForm1.tbTimeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CnTimerList1.Items[0].Enabled := False;
end;

procedure TForm1.tbTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CtrlWinamp.WACurrentTime := tbTime.Position;
  CnTimerList1.Items[0].Enabled := True;
end;

procedure TForm1.btnEnabledWAWindowClick(Sender: TObject);
begin
  CtrlWinamp.EnabledWAWindow := True;
end;

procedure TForm1.btnDisabledWAWindowClick(Sender: TObject);
begin
  CtrlWinamp.EnabledWAWindow := False;
end;

procedure TForm1.btnEnabledEQClick(Sender: TObject);
begin
  if CtrlWinamp.EQData[EQEnabled] = 0 then
  begin
    btnEnabledEQ.Caption := 'EQ On';
    CtrlWinamp.EQData[EQEnabled] := 1;
  end
  else begin
    btnEnabledEQ.Caption := 'EQ Off';
    CtrlWinamp.EQData[EQEnabled] := 0;
  end;
end;

procedure TForm1.btnEQAutoLoadClick(Sender: TObject);
begin
  if CtrlWinamp.EQData[EQAutoLoad] = 0 then
  begin
    btnEQAutoLoad.Caption := 'EQ Load On';
    CtrlWinamp.EQData[EQAutoLoad] := 1;
  end
  else begin
    btnEQAutoLoad.Caption := 'EQ Load Off';
    CtrlWinamp.EQData[EQAutoLoad] := 0;
  end;
end;

procedure TForm1.tbEQ10Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQPreAmp] := tbEQ10.Position;
end;

procedure TForm1.tbEQ0Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ60hz] := tbEQ0.Position;
end;

procedure TForm1.tbEQ1Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ170hz] := tbEQ1.Position;
end;

procedure TForm1.tbEQ2Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ310hz] := tbEQ2.Position;
end;

procedure TForm1.tbEQ3Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ600hz] := tbEQ3.Position;
end;

procedure TForm1.tbEQ4Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ1k] := tbEQ4.Position;
end;

procedure TForm1.tbEQ5Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ3k] := tbEQ5.Position;
end;

procedure TForm1.tbEQ6Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ6k] := tbEQ6.Position;
end;

procedure TForm1.tbEQ7Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ12k] := tbEQ7.Position;
end;

procedure TForm1.tbEQ8Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ14k] := tbEQ8.Position;
end;

procedure TForm1.tbEQ9Change(Sender: TObject);
begin
  CtrlWinamp.EQData[EQ16k] := tbEQ9.Position;
end;

procedure TForm1.btnAboutClick(Sender: TObject);
var
  strAbout: string;
begin
  strAbout := '程序名称: Winamp 控制器示例程序' + #13#10 +
              '程序作者: 小冬(kendling)(kendling@21cn.com)' + #13#10 +
              '程序说明: 使用TCnWinampCtrl控件的示例' + #13#10 + #13#10 +
              '下载网站: http://CnPack.org' + #13#10 +
              '技术支持: master@CnPack.org' + #13#10 + #13#10 +
              '(C)Copyright 2001-2011 CnPack 开发组';
  ShowMessage(strAbout);
end;

end.
