{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnDancingLinks;
{* |<PRE>
================================================================================
* 软件名称：CnPack 公共单元
* 单元名称：实现基于十字双向循环链表的稀疏矩阵，并且基于此实现舞蹈链表
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元为描述稀疏矩阵实现了十字双向循环链表，支持跨平台。
*           并在此基础上实现了舞蹈链表的快速删除/恢复行列的机制。
*           但尚未抽象出回溯递归操作。
* 开发平台：PWinXP + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2015.05.29 V1.0 by LiuXiao
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Contnrs;

type
  ECnCrossLinkedMatrixException = class(Exception);

//==============================================================================
// 十字双向链表节点实现类
//==============================================================================

  TCnCrossLinkedNode = class(TObject)
  {* 十字双向链表节点实现类}
  private
    FUp: TCnCrossLinkedNode;
    FLeft: TCnCrossLinkedNode;
    FRight: TCnCrossLinkedNode;
    FDown: TCnCrossLinkedNode;
    FColumn: Integer;
    FRow: Integer;
    FData: Integer;
    FText: string;
  public
    property Left: TCnCrossLinkedNode read FLeft write FLeft;
    {* 本节点本行左边的节点，如本行只有本节点，则此指向节点自身}
    property Right: TCnCrossLinkedNode read FRight write FRight;
    {* 本节点本行右边的节点，如本行只有本节点，则此指向节点自身}
    property Up: TCnCrossLinkedNode read FUp write FUp;
    {* 本节点本列上边的节点，如本列只有本节点，则此指向节点自身}
    property Down: TCnCrossLinkedNode read FDown write FDown;
    {* 本节点本列下边的节点，如本列只有本节点，则此指向节点自身}

    property Column: Integer read FColumn;
    {* 本节点所在的列，从 0 开始}
    property Row: Integer read FRow;
    {* 本节点所在的行，从 0 开始}

    property Data: Integer read FData write FData;
    {* 用以保存一整数的属性，类似于 Tag}
    property Text: string read FText write FText;
    {* 用以保存一字符串的属性}
  end;

  TCnCrossLinkedNodeClass = class of TCnCrossLinkedNode;

//==============================================================================
// 十字双向循环链表实现的稀疏矩阵类
//==============================================================================

  TCnCrossLinkedMatrix = class(TObject)
  {* 十字双向循环链表实现的稀疏矩阵类}
  private
    FCount: Integer;  
    FColCount: Integer;
    FRowCount: Integer;
    FNodeClass: TCnCrossLinkedNodeClass;

    FColumnHeads: TObjectList;  // 列头指针
    FRowHeads: TObjectList;     // 行头指针
    FOnTravelNode: TNotifyEvent;

    function GetCells(Col, Row: Integer): TCnCrossLinkedNode;
    function CreateNode: TCnCrossLinkedNode;
    function GetColumnHead(Col: Integer): TCnCrossLinkedNode;
    function GetRowHead(Row: Integer): TCnCrossLinkedNode;
  protected
    procedure DoTravelNode(Node: TCnCrossLinkedNode); virtual;
  public
    constructor Create(AColCount: Integer; ARowCount: Integer;
      NodeClass: TCnCrossLinkedNodeClass = nil); virtual;
    destructor Destroy; override;

    function InsertNode(ACol, ARow: Integer): TCnCrossLinkedNode;
    {* 在稀疏矩阵的指定行列位置插入一节点并返回此节点，如此位置已存在则返回 nil}
    function ExtractNode(ACol, ARow: Integer): TCnCrossLinkedNode;
    {* 在稀疏矩阵的指定行列位置解出节点并返回此节点，如此位置无节点则返回 nil}
    procedure RemoveNode(ACol, ARow: Integer);
    {* 删除稀疏矩阵指定行列位置的节点}

    procedure TravelByRow;
    {* 根据行遍历}
    procedure TravelByCol;
    {* 根据列遍历}
    procedure ExpandRow(ExpandCount: Integer = 1);
    {* 动态扩大行数}
    procedure ExpandCol(ExpandCount: Integer = 1);
    {* 动态扩大列数}

    property RowCount: Integer read FRowCount;
    {* 稀疏矩阵的行数}
    property ColCount: Integer read FColCount;
    {* 稀疏矩阵的列数}
    property Cells[Col, Row: Integer]: TCnCrossLinkedNode read GetCells;
    {* 根据行、列数访问稀疏矩阵的单元，行列均从 0 开始}
    property RowHead[Row: Integer]: TCnCrossLinkedNode read GetRowHead;
    {* 根据行访问行头元素}
    property ColumnHead[Col: Integer]: TCnCrossLinkedNode read GetColumnHead;
    {* 根据列访问列头元素}
    property Count: Integer read FCount;
    {* 元素个数}
    property OnTravelNode: TNotifyEvent read FOnTravelNode write FOnTravelNode;
    {* 遍历时触发的事件}
  end;

//==============================================================================
// 舞蹈链表实现类
//==============================================================================

  TCnDancingLinks = class(TCnCrossLinkedMatrix)
  {* 舞蹈链表实现类，增加了快速删除/还原行列的方法}
  public
    function ExtractRow(ARow: Integer): TCnCrossLinkedNode;
    {* 拆除一行，返回该行的行头元素，该行所有元素与其他行脱离关系}
    function RestoreRow(ARowHead: TCnCrossLinkedNode): Boolean;
    {* 将 ExtractRow 拆出的行重新放置回原位}
    function ExtractColumn(ACol: Integer): TCnCrossLinkedNode;
    {* 拆除一列，返回该列的列头元素，该行所有元素与其他列脱离关系}
    function RestoreColumn(ColHead: TCnCrossLinkedNode): Boolean;
    {* 将 ExtractColumn 拆出的行重新放置回原位}
  end;

implementation

{ TCnCrossLinkedMatrix }

constructor TCnCrossLinkedMatrix.Create(AColCount, ARowCount: Integer;
  NodeClass: TCnCrossLinkedNodeClass);
var
  I: Integer;
begin
  inherited Create;
  if (AColCount <= 0) or (ARowCount <= 0) then
    raise ECnCrossLinkedMatrixException.Create('Error Column/Row Count.');

  FColCount := AColCount;
  FRowCount := ARowCount;
  if NodeClass = nil then
    FNodeClass := TCnCrossLinkedNode
  else
    FNodeClass := NodeClass;

  FColumnHeads := TObjectList.Create(False);
  FRowHeads := TObjectList.Create(False);

  for I := 0 to RowCount - 1 do
    FRowHeads.Add(nil);
  for I := 0 to ColCount - 1 do
    FColumnHeads.Add(nil);
end;

function TCnCrossLinkedMatrix.CreateNode: TCnCrossLinkedNode;
begin
  try
    Result := TCnCrossLinkedNode(FNodeClass.NewInstance);
    Result.Create;
  except
    Result := nil;
  end;
end;

destructor TCnCrossLinkedMatrix.Destroy;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  if FColumnHeads <> nil then
  begin
    for I := 0 to FColumnHeads.Count - 1 do
    begin
      Head := TCnCrossLinkedNode(FColumnHeads[I]);
      if Head <> nil then
      begin
        // 释放本列
        P := Head;
        repeat
          Q := P.Down;
          P.Free;
          P := Q;
        until (P = Head) or (P = nil);
      end;
    end;
  end;
  inherited;
end;

procedure TCnCrossLinkedMatrix.DoTravelNode(Node: TCnCrossLinkedNode);
begin
  if Assigned(FOnTravelNode) then
    FOnTravelNode(Node);
end;

procedure TCnCrossLinkedMatrix.ExpandCol(ExpandCount: Integer);
var
  I: Integer;
begin
  if ExpandCount <= 0 then
    raise ECnCrossLinkedMatrixException.Create('Invalid Expand Count.');

  Inc(FColCount, ExpandCount);
  for I := 1 to ExpandCount do
    FColumnHeads.Add(nil);
end;

procedure TCnCrossLinkedMatrix.ExpandRow(ExpandCount: Integer);
var
  I: Integer;
begin
  if ExpandCount <= 0 then
    raise ECnCrossLinkedMatrixException.Create('Invalid Expand Count.');

  Inc(FRowCount, ExpandCount);
  for I := 1 to ExpandCount do
    FRowHeads.Add(nil);
end;

function TCnCrossLinkedMatrix.ExtractNode(ACol, ARow: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
begin
  Result := nil;
  if FColumnHeads[ACol] <> nil then  // 从指定列搜索
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[ACol]);
    P := Head;
    repeat
      if P.Row = ARow then
      begin
        Result := P;

        // 开始解开列方向的 P
        if (P = Head) and (P.Up = P) and (P.Down = P) then
        begin
          // 本列只有 Head 一个，直接列头清空
          FColumnHeads[ACol] := nil;
        end
        else
        begin
          if P = Head then // P 在列头，需要更新列头
            FColumnHeads[ACol] := P.Down;
          P.Up.Down := P.Down;
          P.Down.Up := P.Up;
        end;

        // 开始解开行方向的 P
        Head := TCnCrossLinkedNode(FRowHeads[ARow]);
        if (P = Head) and (P.Left = P) and (P.Right = P) then
        begin
          // 本行只有 Head 一个，直接行头清空
          FRowHeads[ARow] := nil;
        end
        else
        begin
          if P = Head then // P 在行头，需要更行列头
            FRowHeads[ARow] := P.Right;
          P.Left.Right := P.Right;
          P.Right.Left := P.Left;
        end;

        Dec(FCount);
        Exit;
      end;
      P := P.Down;
    until (P = Head) or (P = nil);
  end;
end;

function TCnCrossLinkedMatrix.GetCells(Col, Row: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
begin
  Result := nil;
  if FColumnHeads[Col] <> nil then  // 从指定列搜索
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[Col]);
    P := Head;
    repeat
      if P.Row = Row then
      begin
        Result := P;
        Exit;
      end;
      P := P.Down;
    until (P = Head) or (P = nil);
  end;
end;

function TCnCrossLinkedMatrix.GetColumnHead(Col: Integer): TCnCrossLinkedNode;
begin
  if (Col < 0) or (Col >= FColCount) then
    raise ECnCrossLinkedMatrixException.Create('Invalid Column Index.');

  Result := TCnCrossLinkedNode(FColumnHeads[Col]);
end;

function TCnCrossLinkedMatrix.GetRowHead(Row: Integer): TCnCrossLinkedNode;
begin
  if (Row < 0) or (Row >= FColCount) then
    raise ECnCrossLinkedMatrixException.Create('Invalid Row Index.');

  Result := TCnCrossLinkedNode(FRowHeads[Row]);
end;

function TCnCrossLinkedMatrix.InsertNode(ACol, ARow: Integer): TCnCrossLinkedNode;
var
  P, Head: TCnCrossLinkedNode;
  InsertColSuccess, InsertRowSuccess: Boolean;
begin
  Result := nil;
  if (ACol < 0) or (ARow < 0) or (ACol >= FColCount) or (ARow >= FRowCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Column/Row Index.');

  if Cells[ACol, ARow] <> nil then // 已经存在
    Exit;

  InsertRowSuccess := False;
  InsertColSuccess := False;
  Result := CreateNode;
  Result.FColumn := ACol;
  Result.FRow := ARow;

  try
    if FRowHeads[ARow] = nil then // 本行为空，直接左右链上自己
    begin
      Result.Left := Result;
      Result.Right := Result;
      FRowHeads[ARow] := Result;
    end
    else // 本行不为空，找到插入地点
    begin
      Head := TCnCrossLinkedNode(FRowHeads[ARow]);
      P := Head;
      repeat
        if P.Column = ACol then
        begin
          // 本行的此列位置已被占用，直接释放退出
          Exit;
        end
        else if P.Column > ACol then
          Break;

        P := P.Right;
      until (P = Head) or (P = nil);

      if (P = Head) and (Head.Column < ACol) then
      begin
        // 找到头了都没找到行索引大于要插入的行的，插在本行最末
        P := P.Left; // P 此时绕回去是行尾节点
        P.Right := Result;
        Result.Left := P;
        Result.Right := Head;
        Head.Left := Result;
      end
      else
      begin
        // P 是行索引大于 ARow 的第一个，应该插在 P 左边与 P 之间
        if Head = P then // P 是行头，更新行头
          FRowHeads[ARow] := Result;

        // 插 P 左边
        Result.Left := P.Left;
        Result.Right := P;
        P.Left.Right := Result;
        P.Left := Result;
      end;
    end;
    InsertRowSuccess := True;

    if FColumnHeads[ACol] = nil then // 本列为空，直接上下链上自己
    begin
      Result.Up := Result;
      Result.Down := Result;
      FColumnHeads[ACol] := Result;
    end
    else // 本列不为空，找到插入地点
    begin
      Head := TCnCrossLinkedNode(FColumnHeads[ACol]);
      P := Head;
      repeat
        if P.Row = ARow then
        begin
          // 本列的此行位置已被占用，要恢复并删掉 Result 已经插入的列
          // 但最上做了检测判断，所以此处理论上进不来，无需考虑复杂的释放行问题
          Exit;
        end
        else if P.Row > ARow then
          Break;

        P := P.Down;
      until (P = Head) or (P = nil);

      if (P = Head) and (P.Row < ARow) then
      begin
        // 找到头了都没找到列索引大于要插入的列的，插在本列最末
        P := P.Up; // P 此时绕回去是列尾节点
        P.Down := Result;
        Result.Up := P;
        Result.Down := Head;
        Head.Up := Result;
      end
      else
      begin
        // P 是列索引大于 ACol 的第一个，应该插在 P 左边与 P 之间
        if P = Head then // P 是列头，更新列头
          FColumnHeads[ACol] := Result;

        // 插 P 上边
        Result.Up := P.Up;
        Result.Down := P;
        P.Up.Down := Result;
        P.Up := Result;
      end;
    end;
    InsertColSuccess := True;
  finally
    if not InsertColSuccess and not InsertRowSuccess then
      FreeAndNil(Result)
    else
      Inc(FCount);
  end;
end;

procedure TCnCrossLinkedMatrix.RemoveNode(ACol, ARow: Integer);
begin
  ExtractNode(ACol, ARow).Free;
end;

procedure TCnCrossLinkedMatrix.TravelByCol;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  for I := 0 to FColumnHeads.Count - 1 do
  begin
    Head := TCnCrossLinkedNode(FColumnHeads[I]);
    if Head <> nil then
    begin
      P := Head;
      repeat
        Q := P.Down;
        DoTravelNode(P);
        P := Q;
      until (P = Head) or (P = nil);
    end;
  end;
end;

procedure TCnCrossLinkedMatrix.TravelByRow;
var
  I: Integer;
  P, Q, Head: TCnCrossLinkedNode;
begin
  for I := 0 to FRowHeads.Count - 1 do
  begin
    Head := TCnCrossLinkedNode(FRowHeads[I]);
    if Head <> nil then
    begin
      P := Head;
      repeat
        Q := P.Right;
        DoTravelNode(P);
        P := Q;
      until (P = Head) or (P = nil);
    end;
  end

end;

{ TCnDancingLinks }

function TCnDancingLinks.ExtractColumn(ACol: Integer): TCnCrossLinkedNode;
var
  P: TCnCrossLinkedNode;
  Row: Integer;
begin
  if (ACol < 0) or (ACol >= ColCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Column Index.');

  Result := TCnCrossLinkedNode(FColumnHeads[ACol]);
  if Result = nil then
    Exit;

  // 遍历 Result 指向的列元素，逐个从行里解开
  P := Result;
  repeat
    Row := P.Row;
    if (P.Left = P) and (P.Right = P) and (FRowHeads[Row] = P) then // 本行只有一个，直接清行头
      FRowHeads[Row] := nil
    else
    begin
      // P 是行头，则让行头指向右一个
      if FRowHeads[Row] = P then
        FRowHeads[Row] := P.Right;
      // 解开 P
      P.Left.Right := P.Right;
      P.Right.Left := P.Left;
    end;
    
    Dec(FCount);
    P := P.Down;
  until (P = Result) or (P = nil);
  // 再摘除整列
  FColumnHeads[ACol] := nil;
end;

function TCnDancingLinks.ExtractRow(ARow: Integer): TCnCrossLinkedNode;
var
  P: TCnCrossLinkedNode;
  Col: Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) then
    raise ECnCrossLinkedMatrixException.Create('Error Row Index.');

  Result := TCnCrossLinkedNode(FRowHeads[ARow]);
  if Result = nil then
    Exit;

  // 遍历 Result 指向的行元素，逐个从列里解开
  P := Result;
  repeat
    Col := P.Column;
    if (P.Up = P) and (P.Down = P) and (FColumnHeads[Col] = P) then // 本列只有一个，直接清列头
      FColumnHeads[Col] := nil
    else
    begin
      // P 是列头，则让列头指向右一个
      if FColumnHeads[Col] = P then
        FColumnHeads[Col] := P.Down;
      // 解开 P
      P.Up.Down := P.Down;
      P.Down.Up := P.Up;
    end;

    Dec(FCount);
    P := P.Right;
  until (P = Result) or (P = nil);
  // 再摘除整行
  FRowHeads[ARow] := nil;
end;

function TCnDancingLinks.RestoreColumn(ColHead: TCnCrossLinkedNode): Boolean;
var
  Row, Col: Integer;
  P: TCnCrossLinkedNode;
begin
  Result := False;
  if ColHead = nil then
    Exit;

  Col := ColHead.Column;
  if (Col < 0) or (Col >= ColCount) then
    Exit;

  if FColumnHeads[Col] <> nil then // 此列已存在，无法再次插入
    Exit;

  // 重新将本列元素插入
  FColumnHeads[Col] := ColHead; // 列头指向此列首元素
  P := ColHead;
  repeat
    // 对每一个列元素，重建在行中的链接关系
    Row := P.Row;
    if FRowHeads[Row] = nil then
    begin
      // 本行无元素，直接链至行头
      FRowHeads[Row] := P;
      P.Left := P;
      P.Right := P;
    end
    else
    begin
      // 恢复 P
      P.Left.Right := P;
      P.Right.Left := P;
      // 如果 P 在行首，则更新行头指针
      if P.Column < TCnCrossLinkedNode(FRowHeads[Row]).Column then
        FRowHeads[Row] := P;
    end;

    Inc(FCount);
    P := P.Down;
  until (P = ColHead) or (P = nil);
end;

function TCnDancingLinks.RestoreRow(ARowHead: TCnCrossLinkedNode): Boolean;
var
  Row, Col: Integer;
  P: TCnCrossLinkedNode;
begin
  Result := False;
  if ARowHead = nil then
    Exit;

  Row := ARowHead.Row;
  if (Row < 0) or (Row >= RowCount) then
    Exit;

  if FRowHeads[Row] <> nil then // 此行已存在，无法再次插入
    Exit;

  // 重新将本行元素插入
  FRowHeads[Row] := ARowHead; // 列头指向此列首元素
  P := ARowHead;
  repeat
    // 对每一个行元素，重建在列中的链接关系
    Col := P.Column;
    if FColumnHeads[Col] = nil then
    begin
      // 本列无元素，直接链至列头
      FColumnHeads[Col] := P;
      P.Up := P;
      P.Down := P;
    end
    else
    begin
      // 恢复 P
      P.Up.Down := P;
      P.Down.Up := P;
      // 如果 P 在列首，则更新列头指针
      if P.Row < TCnCrossLinkedNode(FColumnHeads[Col]).Row then
        FColumnHeads[Col] := P;
    end;

    Inc(FCount);
    P := P.Right;
  until (P = ARowHead) or (P = nil);
end;

end.
