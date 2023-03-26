unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnObjAuto, StdCtrls, TypInfo;

type
  {$MethodInfo ON}
  TDemoClass = class(TPersistent)
  public
    function Add(X,Y: Integer): Integer;
    procedure ShowCaption(AButton: TButton);
  end;
  {$MethodInfo OFF}

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  miArr: TMethodInfoArray;
  mi: PMethodInfoHeader;
  demo: TDemoClass;
  retInfo: PReturnInfo;
  piArr: TParamInfoArray;
  pi: PParamInfo;
  i, j: Integer;
begin
  demo:= TDemoClass.Create;
  miArr := GetMethods(demo.ClassType);
  for i:=0 to length(miArr)-1 do
  begin
    mi := miArr[i];
    memo1.Lines.Add('MethodName: '+mi.Name);
    retInfo := GetReturnInfo(demo, mi.Name);
    if retInfo.ReturnType = nil then
      memo1.Lines.Add('  Type: procedure')
    else
    begin
      memo1.Lines.Add('  Type: function');
      memo1.Lines.Add('  Returns: '+ retInfo.ReturnType^.Name);
    end;
    piArr := GetParams(demo, mi.Name);
    if piArr <> nil then
    begin
      for j:=0 to length(piArr)-1 do
      begin
        pi := piArr[j];
        if UPPERCASE(pi.Name) = 'SELF' then
          Continue;
        memo1.Lines.Add('    ParamName: '+pi.Name);
        memo1.Lines.Add('    ParamType: '+pi.ParamType^.Name);
        memo1.Lines.Add('    ParamKind: '+GetEnumName(typeinfo(TTypeKind),ord(pi.ParamType^.Kind)))
      end;
    end;
    memo1.Lines.Add('--------------------------');
  end;
  demo.Free;
end;

{ TDemoClass }

function TDemoClass.Add(X, Y: Integer): Integer;
begin
  Result := X+Y;
end;

procedure TDemoClass.ShowCaption(AButton: TButton);
begin
  ShowMessage(AButton.Caption);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  demo: TDemoClass;
  mi: PMethodInfoHeader;
  a,b, c: integer;
begin
  demo:= TDemoClass.Create;
  mi := CnObjAuto.GetMethodInfo(demo, 'Add');
  a := 20;
  b := 40;
  c := ObjectInvoke(demo,mi,[1,2],[a,b]);
  ShowMessage(InttoStr(c));
  demo.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  demo: TDemoClass;
  mi: PMethodInfoHeader;
begin
  demo:= TDemoClass.Create;
  mi := CnObjAuto.GetMethodInfo(demo, 'ShowCaption');
  ObjectInvoke(demo,mi,[1],[Integer(Button1)]);
  demo.Free;
end;

end.
