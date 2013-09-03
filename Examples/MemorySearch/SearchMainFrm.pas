unit SearchMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnMemorySearch;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure CnSearch1SearchComplete(const Status: Boolean);
    procedure CnSearch1SearchError(const Status: TErrList; Msg: string);
    procedure CnSearch1SearchFind(const Index: Integer; pData: TDataItem);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    CnSearch1: TCnMemorySearch;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
const
   _Button1ClickTag: TtagData = ( $42, $75, $74, $74, $6f, $6e, $31, $43, $6c, $69, $63, $6b, $53, $74, $72, $00);
   //  A:string='Button1ClickStr';
   _Button2ClickTag: TtagData = ( $42, $75, $74, $74, $6f, $6e, $32, $43, $6c, $69, $63, $6b, $53, $74, $72, $00);
   //  A:string='Button1ClickStr';
begin
  CnSearch1.Directory := ExtractFilePath(ParamStr(0));
  CnSearch1.SetCount(2);
  CnSearch1.DataList[0].FileName :='Target.exe';
  CnSearch1.DataList[0].ConstStr:='Button1Click Method Address:' ;

  CnSearch1.DataList[0].MemoryOffset := 0;
  CnSearch1.DataList[0].ResultType := rtPointer;
  CnSearch1.DataList[0].ResultData := 0;
  CnSearch1.DataList[0].TagCount := 1;
  CnSearch1.DataList[0].ModuleTag[0].TagData := _Button1ClickTag;
  CnSearch1.DataList[0].ModuleTag[0].Offset := -$10;
  CnSearch1.DataList[0].ModuleTag[0].Len := 15;

  CnSearch1.DataList[1].FileName :='Target.exe';
  CnSearch1.DataList[1].ConstStr:='Button2Click Method Address:' ;

  CnSearch1.DataList[1].MemoryOffset := 0;
  CnSearch1.DataList[1].ResultType := rtPointer;
  CnSearch1.DataList[1].ResultData := 0;
  CnSearch1.DataList[1].TagCount := 1;
  CnSearch1.DataList[1].ModuleTag[0].TagData := _Button2ClickTag;
  CnSearch1.DataList[1].ModuleTag[0].Offset := -$10;
  CnSearch1.DataList[1].ModuleTag[0].Len := 15;

  CnSearch1.StartSearch:=True;
end;

procedure TForm1.CnSearch1SearchComplete(const Status: Boolean);
begin
  Memo1.Lines.Add(Format('%s %x ', [CnSearch1.DataList[0].ConstStr, CnSearch1.DataList[0].ResultData]));
  Memo1.Lines.Add(Format('%s %x ', [CnSearch1.DataList[1].ConstStr, CnSearch1.DataList[1].ResultData]));
end;

procedure TForm1.CnSearch1SearchError(const Status: TErrList; Msg: string);
begin
  case Status of
    elFileErr: Memo1.Lines.Add('ÎÄ¼þËÑË÷´íÎó:' + Msg);
    elModuleErr: Memo1.Lines.Add('ÄÚ´æËÑË÷´íÎó:' + Msg);
  end;
end;

procedure TForm1.CnSearch1SearchFind(const Index: Integer; pData: TDataItem);
begin
  Memo1.Lines.Add(Format('%d#ÕÒµ½ ', [Index]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnSearch1 := TCnMemorySearch.Create(Self);
  CnSearch1.OnSearchFind := CnSearch1SearchFind;
  CnSearch1.OnSearchComplete := CnSearch1SearchComplete;
  CnSearch1.OnSearchError := CnSearch1SearchError;
  CnSearch1.SearchMethod := smlSearchFile;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CnSearch1.Free;
end;

end.

