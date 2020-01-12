{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit TestSkipListUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, CnSkipList, StdCtrls, ComCtrls, ExtCtrls;

type
  TSkipListTestForm = class(TForm)
    Grid: TStringGrid;
    btnShow: TButton;
    btnAdd1: TButton;
    btnAdd2: TButton;
    btnAdd3: TButton;
    edtValue: TEdit;
    udValue: TUpDown;
    btnAdd: TButton;
    btnRandom: TButton;
    btnDel: TButton;
    bvl1: TBevel;
    bvl2: TBevel;
    bvl3: TBevel;
    btnSearch: TButton;
    bvl4: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnAdd1Click(Sender: TObject);
    procedure btnAdd2Click(Sender: TObject);
    procedure btnAdd3Click(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FSkipList: TCnSkipList;
    procedure UpdateSkipListToGrid;
    procedure AddValue(Value: Integer);
  public

  end;

var
  SkipListTestForm: TSkipListTestForm;

implementation

{$R *.dfm}

function Comp(const Value: Pointer; const Node: TCnSkipListNode): Integer;
var
  P1, P2: Integer;
begin
//  if Node = nil then
//  begin
//    Result := -1;
//    Exit;
//  end;

  P1 := Integer(Value);
  P2 := Integer(Node.Data);

  if P1 > P2 then
    Result := 1
  else if P1 = P2 then
    Result := 0
  else
    Result := -1;
end;

procedure TSkipListTestForm.FormCreate(Sender: TObject);
begin
  FSkipList := TCnSkipList.Create(Comp);
  FSkipList.Head.Text := 'Head';
  UpdateSkipListToGrid;
end;

procedure TSkipListTestForm.FormDestroy(Sender: TObject);
begin
  FSkipList.Free;
end;

procedure TSkipListTestForm.UpdateSkipListToGrid;
var
  I, Col: Integer;
  P: TCnSkipListNode;
begin
  Grid.RowCount := FSkipList.MaxLevel + 1; // 0 到 MaxLevel 层
  Grid.ColCount := FSkipList.Count + 2;    // 最左一列行号，最右一列 nil

  for I := 0 to Grid.RowCount - 1 do
    for Col := 0 to Grid.ColCount - 1 do
      Grid.Cells[Col, I] := '';

  // 设左边的层次数
  for I := 0 to Grid.RowCount - 1 do
    Grid.Cells[0, I] := IntToStr(Grid.RowCount - 1 - I);

  // 遍历访问，Head 对应列 1，
  Col := 1;
  P := FSkipList.Head;
  while P <> nil do
  begin
    if P = FSkipList.Head then
    begin
      for I := 0 to Grid.RowCount - 1 do
        Grid.Cells[Col, I] := '>';
    end
    else
    begin
      // P 的数据要放 Col 列中
      for I := 0 to P.Level do
      begin
        if (P.Forwards[I] <> nil) or (I = 0) then    // i = 0 表示最下层，也就是 RowCount - 1 行
          Grid.Cells[Col, Grid.RowCount - 1 - I] := IntToStr(Integer(P.Data));
      end;
    end;
    Inc(Col);
    P := P.Forwards[0];
  end;

  for I := 0 to Grid.RowCount - 1 do
    Grid.Cells[Col, Grid.RowCount - 1 - I] := 'nil';
end;

procedure TSkipListTestForm.btnShowClick(Sender: TObject);
begin
  UpdateSkipListToGrid;
end;

procedure TSkipListTestForm.btnAdd1Click(Sender: TObject);
begin
  AddValue(1);
end;

procedure TSkipListTestForm.AddValue(Value: Integer);
var
  Node: TCnSkipListNode;
begin
  Node := FSkipList.Insert(Pointer(Value));
  if Node <> nil then
  begin
    Node.Data := Pointer(Value);
    Node.Text := IntToStr(Value);
    UpdateSkipListToGrid;
  end
  else
    ShowMessage('Insert Failed.');
end;

procedure TSkipListTestForm.btnAdd2Click(Sender: TObject);
begin
  AddValue(2);
end;

procedure TSkipListTestForm.btnAdd3Click(Sender: TObject);
begin
  AddValue(3);
end;

procedure TSkipListTestForm.btnAddClick(Sender: TObject);
begin
  AddValue(udValue.Position);
end;

procedure TSkipListTestForm.btnRandomClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to 50 do
    AddValue(Trunc(Random * 1000) - 400);
end;

procedure TSkipListTestForm.btnDelClick(Sender: TObject);
var
  I, E: Integer;
  S: string;
begin
  S := Grid.Cells[Grid.Col, Grid.Row];
  if S = '' then
  begin
    for I := Grid.Row to Grid.RowCount - 1 do
    begin
      S := Grid.Cells[Grid.Col, I];
      if S <> '' then
        Break;
    end;
  end;

  Val(S, I, E);
  if E <> 0 then
  begin
    ShowMessage('Can NOT Delete: ' + S);
    Exit;
  end;

  if FSkipList.Delete(Pointer(I)) then
  begin
    ShowMessage('Delete OK. ' + S);
    UpdateSkipListToGrid;
  end
  else
    ShowMessage('Delete Failed. ' + S);
end;

procedure TSkipListTestForm.btnSearchClick(Sender: TObject);
var
  S: string;
  I, E: Integer;
  Node: TCnSkipListNode;
begin
  S := InputBox('Search', 'Enter a Value:', '0');
  Val(S, I, E);
  if E <> 0 then
  begin
    ShowMessage('Can NOT Search: ' + S);
    Exit;
  end;

  Node := FSkipList.Search(Pointer(I));
  if Node <> nil then
    ShowMessage('Found.')
  else
    ShowMessage('NOT Found.');
end;

end.
