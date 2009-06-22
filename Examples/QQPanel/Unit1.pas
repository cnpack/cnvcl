unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CnFMForm, CnFMQQPane, StdCtrls, WinSkinData, ComCtrls,
  RaCalendar, RaMonthCalendar;

type
  TForm1 = class(TForm)
    CnFMQQPanel1: TCnFMQQPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure CnFMQQPanel1MemberDblClick(Sender: TObject; AData: Pointer);
  private
    { Private declarations }
  public
    procedure OnIcoClick(Sender: TObject; AData: Pointer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
  img: string;
  ico: TCnFMQQIco;
  p: TCnFMQQPerson;
  dt: TCnFMQQIcoData;
begin

  img := ExtractFilePath(ParamStr(0)) + 'Img\';
  CnFMQQPanel1.AddGroup('Group[1]');
  CnFMQQPanel1.AddGroup('Group[2]');

  p:= TCnFMQQPerson.Create;
  p.UserID := '1111111111';
  p.UserName := '测试人员';
  p.UserDesc := '2222222222';
  p.UserHead := img+'Head.bmp';
  p.NameColor := clRed;

  dt:= TCnFMQQIcoData.Create;
  dt.IcoID := 'video';
  dt.IcoName := 'video icon';
  dt.IcoDesc := '拥有摄像头，可以进行视频聊天的用户';

  ico := TCnFMQQIco.Create(self);
  ico.NormalIco:=img + 'video.bmp';
  ico.HotIco := img + 'video_hot.bmp';
  ico.Data := dt;
  ico.OnIconClick := OnIcoClick;
  p.AddIcon(ico);

  dt:= TCnFMQQIcoData.Create;
  dt.IcoID := 'qq';
  dt.IcoName := 'qq icon';
  dt.IcoDesc := '可以通过手机聊天的用户';

  ico := TCnFMQQIco.Create(self);
  ico.NormalIco:=img + 'QQ.bmp';
  ico.HotIco := img + 'QQ_HOT.bmp';
  ico.Data := dt;
  p.AddIcon(ico);
  CnFMQQPanel1.Groups[0].QQGroup.AddMember(p);

  p:= TCnFMQQPerson.Create;
  p.UserID := '11111111';
  p.UserName := '测试';
  p.UserDesc := '22222';
  p.UserHead := img+'Head.bmp';
  CnFMQQPanel1.Groups[0].QQGroup.AddMember(p);

  CnFMQQPanel1.Groups[1].QQGroup.AddMember(p);

  CnFMQQPanel1.PackupAll;

end;

procedure TForm1.OnIcoClick(Sender: TObject; AData: Pointer);
begin
  ShowMessage(TCnFMQQIcoData(AData).IcoName);
end;

procedure TForm1.CnFMQQPanel1MemberDblClick(Sender: TObject; AData: Pointer);
begin
  ShowMessage('Double Clicked');
end;

end.
