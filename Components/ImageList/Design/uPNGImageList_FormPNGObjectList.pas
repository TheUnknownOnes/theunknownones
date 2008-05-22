//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPNGImageList_FormPNGObjectList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, ExtCtrls, StdCtrls, PNGImage, ExtDlgs,
  uPNGHelper, uPNGCommon;

type
  TForm_PNGObjectList = class(TForm)
    PaintBox: TPaintBox;
    iml_LV: TImageList;
    pan_Right: TPanel;
    pan_List: TPanel;
    lv_Images: TListView;
    pan_Bottom: TPanel;
    btn_Add: TButton;
    btn_Replace: TButton;
    btn_Delete: TButton;
    btn_DeleteAll: TButton;
    btn_OK: TButton;
    btn_Cancel: TButton;
    btn_Apply: TButton;
    dlg_OpenImage: TOpenPictureDialog;
    btn_MoveBack: TButton;
    btn_MoveForw: TButton;
    rg_ImgSize: TRadioGroup;
    procedure FormShow(Sender: TObject);
    procedure btn_AddClick(Sender: TObject);
    procedure btn_DeleteClick(Sender: TObject);
    procedure btn_DeleteAllClick(Sender: TObject);
    procedure btn_ReplaceClick(Sender: TObject);
    procedure lv_ImagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure btn_ApplyClick(Sender: TObject);
    procedure btn_MoveBackClick(Sender: TObject);
    procedure btn_MoveForwClick(Sender: TObject);
    procedure rg_ImgSizeClick(Sender: TObject);
  private
    FOrigList,
    FWorkList : TPNGObjectList;

    FChanged : Boolean;

    procedure SyncLists;
    procedure SetListChanged(const Value: Boolean);

    procedure SelectItemByImageIndex(AIndex : Integer);

  public
    property ListChanged : Boolean read FChanged write SetListChanged;
  
    class function Execute(var AList : TPNGObjectList) : Boolean;
  end;


var
  ListImageSize : Integer;

implementation

uses Types;

{$R *.dfm}

{ TForm_PNGObjectList }

procedure TForm_PNGObjectList.btn_AddClick(Sender: TObject);
var
  PNG : TPNGObject;
  FileName : String;
begin
  PNG:=TPNGObject.Create;
  try
    if dlg_OpenImage.Execute then
    begin
      for FileName in dlg_OpenImage.Files do
      begin
        PNG.LoadFromFile(FileName);
        FWorkList.Add(PNG);
      end;
      
      SyncLists;
    end;
  finally
    PNG.Free;
  end;

  ListChanged:=true;
end;

procedure TForm_PNGObjectList.btn_ApplyClick(Sender: TObject);
begin
  FOrigList.Assign(FWorkList);
  ListChanged:=false;
end;

procedure TForm_PNGObjectList.btn_DeleteAllClick(Sender: TObject);
begin
  FWorkList.Clear;
  SyncLists;
end;

procedure TForm_PNGObjectList.btn_DeleteClick(Sender: TObject);
var
  Item : TListItem;
  idx : Integer;
begin
  for idx := lv_Images.Items.Count - 1 downto 0 do
  begin
    Item:=lv_Images.Items[idx];
    if Item.Selected then
      FWorkList.Delete(Item.ImageIndex);
  end;

  SyncLists;

  ListChanged:=true;
end;

procedure TForm_PNGObjectList.btn_MoveBackClick(Sender: TObject);
var
  idx : Integer;
begin
  if lv_Images.Selected.ImageIndex>0 then
  begin
    idx:=lv_Images.Selected.ImageIndex-1;
    FWorkList.Move(lv_Images.Selected.ImageIndex, lv_Images.Selected.ImageIndex-1);
    SyncLists;
    SelectItemByImageIndex(idx);
  end;
end;

procedure TForm_PNGObjectList.btn_MoveForwClick(Sender: TObject);
var
  idx : Integer;
begin
  if lv_Images.Selected.ImageIndex<FWorkList.Count-1 then
  begin
    idx:=lv_Images.Selected.ImageIndex+1;
    FWorkList.Move(lv_Images.Selected.ImageIndex, lv_Images.Selected.ImageIndex+1);
    SyncLists;
    SelectItemByImageIndex(idx);
  end;
end;

procedure TForm_PNGObjectList.btn_ReplaceClick(Sender: TObject);
var
  PNG : TPNGObject;
  Item : TListItem;
begin
  if lv_Images.SelCount>0 then
  begin
    if dlg_OpenImage.Execute then
    begin
      PNG:=TPNGObject.Create;
      try
        for Item in lv_Images.Items do
        begin
          if Item.Selected then
          begin
            PNG.LoadFromFile(dlg_OpenImage.Files[0]);
            FWorkList[Item.ImageIndex]:=PNG;
            dlg_OpenImage.Files.Delete(0);

            ListChanged:=true;

            if dlg_OpenImage.Files.Count=0 then
              break;
          end;         
        end;
      finally
        PNG.Free;
        SyncLists;
      end;
    end;
  end;  
end;

class function TForm_PNGObjectList.Execute(var AList: TPNGObjectList): Boolean;
var
  Form : TForm_PNGObjectList;
begin
  Form:=TForm_PNGObjectList.Create(nil);
  try
    Form.FOrigList:=AList;
    Form.FWorkList:=TPNGObjectList.Create;
    Form.FWorkList.Assign(form.FOrigList);

    Result:=IsPositiveResult(form.ShowModal);

    if Result then
      Form.FOrigList.Assign(Form.FWorkList);

    Form.FWorkList.Free;

  finally
    Form.Release;
  end;
end;

procedure TForm_PNGObjectList.FormShow(Sender: TObject);
const
  ImagesSizes : array[0..4] of Integer = (16,22,32,48,64);
var
  ItemIndex,
  idx : Integer;
begin
  ListChanged:=false;

  for idx := Low(ImagesSizes) to High(ImagesSizes) do
  begin
    ItemIndex:=rg_ImgSize.Items.AddObject(Format('%d x %d',[ImagesSizes[idx],ImagesSizes[idx]]), TObject(ImagesSizes[idx]));
    if ImagesSizes[idx]=ListImageSize then
      rg_ImgSize.ItemIndex:=ItemIndex;
  end;

  rg_ImgSizeClick(Self);
end;

procedure TForm_PNGObjectList.lv_ImagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  PaintBox.Invalidate;

  if Selected and Assigned(Item) then
  begin
    btn_MoveForw.Enabled:=true;
    btn_MoveBack.Enabled:=true;
  end
  else
  begin
    btn_MoveForw.Enabled:=false;
    btn_MoveBack.Enabled:=false;
  end;
end;

procedure TForm_PNGObjectList.PaintBoxPaint(Sender: TObject);
var
  PNG : TPNGObject;
begin
  DrawTransparentBackground(PaintBox.Canvas);

  if Assigned(lv_Images.Selected) then
  begin
    PNG:=FWorkList[lv_Images.Selected.ImageIndex];

    if (PNG.Width>=PaintBox.Width) or
       (PNG.Height>=PaintBox.Height) then
      PNG.Draw(PaintBox.Canvas, PaintBox.Canvas.ClipRect)
    else
      PaintBox.Canvas.Draw((PaintBox.Width-png.Width) div 2,
                           (PaintBox.Height-png.Height) div 2,
                           png);

  end;
end;

procedure TForm_PNGObjectList.rg_ImgSizeClick(Sender: TObject);
begin
  iml_LV.Width:=Integer(rg_ImgSize.Items.Objects[rg_ImgSize.ItemIndex]);
  iml_LV.Height:=iml_LV.Width;

  ListImageSize:=iml_LV.Width;

  SyncLists;
end;

procedure TForm_PNGObjectList.SelectItemByImageIndex(AIndex: Integer);
var
  idx : Integer;
begin
  for idx := 0 to lv_Images.Items.Count - 1 do
    lv_Images.Items[idx].Selected:=lv_Images.Items[idx].ImageIndex=AIndex;
end;

procedure TForm_PNGObjectList.SetListChanged(const Value: Boolean);
begin
  if FChanged<>Value then
  begin
    FChanged := Value;
    btn_Apply.Enabled:=FChanged;
  end;
end;

procedure TForm_PNGObjectList.SyncLists;
var
  PNG : TPNGObject;
  ContainerBMP,
  BMP : TBitmap;
begin
  lv_Images.Clear;
  iml_LV.Clear;

  BMP:=TBitmap.Create;
  BMP.PixelFormat:=pf32bit;
  BMP.Width:=iml_LV.Width;
  BMP.Height:=iml_LV.Height;

  ContainerBMP:=TBitmap.Create;
  ContainerBMP.PixelFormat:=pf32bit;

  for PNG in FWorkList do
  begin
    BMP.Canvas.Brush.Color:=lv_Images.Color;
    BMP.Canvas.Brush.Style:=bsSolid;
    BMP.Canvas.FillRect(bmp.Canvas.ClipRect);

    if (png.Width>=bmp.Width) or (png.Height>=bmp.Height) then
    begin
      bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), PNG);
    end
    else
      bmp.Canvas.Draw((bmp.Width-png.Width) div 2,
                      (bmp.Height-png.Height) div 2,
                      PNG);


    with lv_Images.Items.Add do
    begin
      ImageIndex:=iml_LV.Add(BMP, nil);
      Caption:=IntToStr(ImageIndex);
    end;
  end;

  ContainerBMP.Free;
  bmp.Free;
end;

initialization
  ListImageSize:=32;

end.
