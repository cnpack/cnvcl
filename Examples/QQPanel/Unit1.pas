unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CnQQPanel, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    CnQQPanel1: TCnQQPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure CnQQPanel1MemberDblClick(Sender: TObject; AData: Pointer);
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
  ico: TCnQQIcon;
  p: TCnQQPerson;
  dt: TCnQQIconData;
begin
  img := ExtractFilePath(ParamStr(0)) + 'Img\';
  CnQQPanel1.AddGroup('Group[1]');
  CnQQPanel1.AddGroup('Group[2]');

  p:= TCnQQPerson.Create;
  p.UserID := '1111111111';
  p.UserName := '测试人员';
  p.UserDesc := '2222222222';
  p.UserHead := img+'Head.bmp';
  p.NameColor := clRed;

  dt:= TCnQQIconData.Create;
  dt.IconID := 'video';
  dt.IconName := 'video icon';
  dt.IconDesc := '拥有摄像头，可以进行视频聊天的用户';

  ico := TCnQQIcon.Create(self);
  ico.NormalIcon:=img + 'video.bmp';
  ico.HotIcon := img + 'video_hot.bmp';
  ico.Data := dt;
  ico.OnIconClick := OnIcoClick;
  p.AddIcon(ico);

  dt:= TCnQQIconData.Create;
  dt.IconID := 'qq';
  dt.IconName := 'qq icon';
  dt.IconDesc := '可以通过手机聊天的用户';

  ico := TCnQQIcon.Create(self);
  ico.NormalIcon:=img + 'QQ.bmp';
  ico.HotIcon := img + 'QQ_HOT.bmp';
  ico.Data := dt;
  p.AddIcon(ico);
  CnQQPanel1.Groups[0].QQGroup.AddMember(p);

  p:= TCnQQPerson.Create;
  p.UserID := '11111111';
  p.UserName := '测试';
  p.UserDesc := '22222';
  p.UserHead := img+'Head.bmp';
  CnQQPanel1.Groups[0].QQGroup.AddMember(p);

  CnQQPanel1.Groups[1].QQGroup.AddMember(p);

  CnQQPanel1.PackupAll;
end;

procedure TForm1.OnIcoClick(Sender: TObject; AData: Pointer);
begin
  ShowMessage(TCnQQIconData(AData).IconName);
end;

procedure TForm1.CnQQPanel1MemberDblClick(Sender: TObject; AData: Pointer);
begin
  ShowMessage('Double Clicked');
end;

end.
