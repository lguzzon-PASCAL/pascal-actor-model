{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 2012 - J. Aldo G. de Freitas Junior

{$mode objfpc}
{$H+}{$M+}

Unit
	ActorMessages;

Interface

Uses
	Classes,
	SysUtils,
	RTTIObjects,
	Contnrs;

Type

	// Internal dispatch message

	TInternalDispatchMessage = Record
		MsgStr : String[255];
		Data : Pointer;
	End;

	// Custom messages

	TCustomActorMessage = Class;
	TCustomActorMessage = Class(TRTTIObject)
	Private
		fSource,
		fDestination : String;
		fTransactionID : Int64;
	Public
		Constructor Create(Const aSource, aDestination : String); Virtual;
		Destructor Destroy; Override;
		Function Clone : TCustomActorMessage; Virtual;
	Published
		Property Source : String Read fSource Write fSource;
		Property Destination : String Read fDestination Write fDestination;
		Property TransactionID : Int64 Read fTransactionID Write fTransactionID;
	End;

	TCustomActorMessageClass = Class Of TCustomActorMessage;

	TCustomEncapsulatedActorMessage = Class(TCustomActorMessage)
	Private
		fEncapsulated : TCustomActorMessage;
	Public
		Constructor Create(Const aSource, aDestination : String); Override;
		Destructor Destroy; Override;
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Encapsulated : TCustomActorMessage Read fEncapsulated Write fEncapsulated;
	End;

	TForeignActorMessage = Class(TCustomEncapsulatedActorMessage);

	TCustomStringActorMessage = Class(TCustomActorMessage)
	Private
		fData : String;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Data : String Read fData Write fData;
	End;

	TCustomNameValueActorMessage = Class(TCustomActorMessage)
	Private
		fName : String;
		fValue : Variant;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Name : String Read fName Write fName;
		Property Value : Variant Read fValue Write fValue;
	End;

	TCustomStreamActorMessage = Class(TCustomActorMessage)
	Private
		fStream : TMemoryStream;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Stream : TMemoryStream Read fStream Write fStream;
	End;

	TCustomObjectReferenceActorMessage = Class(TCustomActorMessage)
	Private
		fObjectReference : TObject;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property ObjectReference : TObject Read fObjectReference Write fObjectReference;
	End;

	TCustomClassReferenceActorMessage = Class(TCustomActorMessage)
	Private
		fClassReference : TClass;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property ClassReference : TClass Read fClassReference Write fClassReference;
	End;

	TCustomNamedObjectReferenceActorMessage = Class(TCustomObjectReferenceActorMessage)
	Private
		fName : String;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Name : String Read fName Write fName;
	End;

	TCustomNamedClassReferenceActorMessage = Class(TCustomClassReferenceActorMessage)
	Private
		fName : String;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property Name : String Read fName Write fName;
	End;

	// Producer/router target related messages

	TSetTargetActorMessage = Class(TCustomStringActorMessage);
	TAddTargetActorMessage = Class(TCustomStringActorMessage);
	TDeleteTargetActorMessage = Class(TCustomStringActorMessage);

	// Instance creation/destruction related messages

	TTerminateActorMessage = Class(TCustomActorMessage);
	TRemoveActorMessage = Class(TCustomActorMessage);

	TCreateInstanceActorMessage = Class(TCustomActorMessage)
	Private
		fInstanceName,
		fClassName : String;
	Public
		Function Clone : TCustomActorMessage; Override;
	Published
		Property NameOfInstance : String Read fInstanceName Write fInstanceName;
		Property NameOfClass : String Read fClassName Write fClassName;
	End;

	TConfigInstanceActorMessage = Class(TCustomNameValueActorMessage);
	TGetConfigInstanceActorMessage = Class(TCustomStringActorMessage);
	TGetConfigInstanceReplyActorMessage = Class(TCustomNameValueActorMessage);

	TCreateInstanceAndConfigActorMessage = Class(TCreateInstanceActorMessage)
	Private
		fMessages : Array Of TCustomActorMessage;
		Function GetMessage(Const aIndex : Integer): TCustomActorMessage;
		Function GetCount: Integer;
	Public
		Function Clone : TCustomActorMessage; Override;
		Function AddMessage(Const aMessage : TCustomActorMessage): TCreateInstanceAndConfigActorMessage;
		Property Messages[aIndex : Integer]: TCustomActorMessage Read GetMessage;
		Property MessageCount: Integer Read GetCount;
	End;
	
	// Actor class registration/unregistration related messages
	TRegisterClassActorMessage = Class(TCustomClassReferenceActorMessage);
	TUnregisterClassActorMessage = Class(TCustomStringActorMessage);

	// Standard Logging
	TCustomLogActorMessage = Class(TCustomStringActorMessage);
	TLogActorMessage = Class(TCustomLogActorMessage);
	TWarningActorMessage = Class(TCustomLogActorMessage);
	TErrorActorMessage = Class(TCustomLogActorMessage);
	TDebugActorMessage = Class(TCustomLogActorMessage);
	TInfoActorMessage = Class(TCustomLogActorMessage);

	TActorMessageClassFactory = Class(TObject)
	Private
		fClasses : TFPHashList;
	Public
		Constructor Create;
		Destructor Destroy; Override;
		Procedure RegisterMessage(Const aClass : TCustomActorMessageClass);
		Function Build(Const aClassName : String): TCustomActorMessage;
	End;

Procedure Init;
Procedure Fini;
Procedure RegisterMessages;

Var
	ActorMessageClassFactory : TActorMessageClassFactory;

Implementation

Var
	lRequestIDSemaphore : TMultiReadExclusiveWriteSynchronizer;
	lLastRequestID : Int64;

Function GetNewRequestID: Int64;
Begin
	lRequestIDSemaphore.BeginWrite;
	Try
		Inc(lLastRequestID);
		Result := lLastRequestID;
	Finally
		lRequestIDSemaphore.EndWrite;
	End;
End;

// TCustomActorMessage

Constructor TCustomActorMessage.Create(Const aSource, aDestination : String);
Begin
	Inherited Create;
	fSource := aSource;
	fDestination := aDestination;
	fTransactionID := GetNewRequestID;
	InitRTTI;
End;

Destructor TCustomActorMessage.Destroy;
Begin
	DoneRTTI;
	Inherited Destroy;
End;

Function TCustomActorMessage.Clone : TCustomActorMessage;
Begin
	Result := ActorMessageClassFactory.Build(Self.ClassName);
	Result.Source := Source;
	Result.Destination := Destination;
	Result.TransactionID := TransactionID;
End;

// TCustomEncapsulatedActorMessage

Constructor TCustomEncapsulatedActorMessage.Create(Const aSource, aDestination : String);
Begin
	Inherited Create(aSource, aDestination);
	fEncapsulated := Nil;
End;

Destructor TCustomEncapsulatedActorMessage.Destroy;
Begin
	If Assigned(fEncapsulated) Then
		FreeAndNil(fEncapsulated);
	Inherited Destroy;
End;

Function TCustomEncapsulatedActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomEncapsulatedActorMessage).Encapsulated := Encapsulated.Clone;
End;

// TCustomStringActorMessage

Function TCustomStringActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomStringActorMessage).Data := Data;
End;

// TCustomNameValueActorMessage
Function TCustomNameValueActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomNameValueActorMessage).Name  := Name;
	(Result As TCustomNameValueActorMessage).Value := Value;
End;

// TCustomStreamActorMessage

Function TCustomStreamActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	Stream.Seek(0, soFromBeginning);
	(Result As TCustomStreamActorMessage).Stream.CopyFrom(Stream, Stream.Size);
End;

// TCustomObjectReferenceActorMessage

Function TCustomObjectReferenceActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomObjectReferenceActorMessage).ObjectReference := ObjectReference;
End;

// TCustomClassReferenceActorMessage

Function TCustomClassReferenceActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomClassReferenceActorMessage).ClassReference := ClassReference;
End;

// TCustomNamedObjectReferenceActorMessage

Function TCustomNamedObjectReferenceActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomNamedObjectReferenceActorMessage).Name := Name;
End;

// TCustomNamedClassReferenceActorMessage

Function TCustomNamedClassReferenceActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCustomNamedClassReferenceActorMessage).Name := Name;
End;

// TCreateInstanceActorMessage

Function TCreateInstanceActorMessage.Clone : TCustomActorMessage;
Begin
	Result := Inherited Clone;
	(Result As TCreateInstanceActorMessage).NameOfInstance := NameOfInstance;
	(Result As TCreateInstanceActorMessage).NameOfClass := NameOfClass;
End;

// TCreateInstanceAndConfigActorMessage

Function TCreateInstanceAndConfigActorMessage.GetMessage(Const aIndex : Integer): TCustomActorMessage;
Begin
	Result := fMessages[aIndex + 1];
End;

Function TCreateInstanceAndConfigActorMessage.GetCount: Integer;
Begin
	Result := Length(fMessages);
End;

Function TCreateInstanceAndConfigActorMessage.Clone : TCustomActorMessage;
Var
	lCtrl : Integer;
Begin
	Result := Inherited Clone;
	For lCtrl := 0 To (Result As TCreateInstanceAndConfigActorMessage).GetCount - 1 Do
		(Result As TCreateInstanceAndConfigActorMessage).AddMessage(GetMessage(lCtrl).Clone);
End;

Function TCreateInstanceAndConfigActorMessage.AddMessage(Const aMessage : TCustomActorMessage): TCreateInstanceAndConfigActorMessage;
Begin
	SetLength(fMessages, Length(fMessages) + 1);
	fMessages[High(fMessages)] := aMessage;
	Result := Self;
End;

// TActorMessageClassFactory

Constructor TActorMessageClassFactory.Create;
Begin
	Inherited Create;
	fClasses := TFPHashList.Create;
End;

Destructor TActorMessageClassFactory.Destroy;
Begin
	FreeAndNil(fClasses);
	Inherited Destroy;
End;

Procedure TActorMessageClassFactory.RegisterMessage(Const aClass : TCustomActorMessageClass);
Begin
	fClasses.Add(aClass.ClassName, Pointer(aClass));
End;

Function TActorMessageClassFactory.Build(Const aClassName : String): TCustomActorMessage;
Var
	lIndex : Integer;
Begin
	Result := Nil;
	lIndex := fClasses.FindIndexOf(aClassName);
	If lIndex >= 0 Then
		Result := TCustomActorMessageClass(fClasses.Items[lIndex]).Create('', '');
End;

Procedure Init;
Begin
	lRequestIDSemaphore := TMultiReadExclusiveWriteSynchronizer.Create;
End;

Procedure Fini;
Begin
	ActorMessageClassFactory.Free;
	FreeAndNil(lRequestIDSemaphore);
End;

Procedure RegisterMessages;
Begin
	ActorMessageClassFactory := TActorMessageClassFactory.Create;
	ActorMessageClassFactory.RegisterMessage(TCustomActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomEncapsulatedActorMessage);
	ActorMessageClassFactory.RegisterMessage(TForeignActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomStringActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNameValueActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomStreamActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomObjectReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomClassReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNamedObjectReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomNamedClassReferenceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TSetTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TAddTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TDeleteTargetActorMessage);
	ActorMessageClassFactory.RegisterMessage(TTerminateActorMessage);
	ActorMessageClassFactory.RegisterMessage(TRemoveActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCreateInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCreateInstanceAndConfigActorMessage);
	ActorMessageClassFactory.RegisterMessage(TConfigInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TGetConfigInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TGetConfigInstanceReplyActorMessage);
	ActorMessageClassFactory.RegisterMessage(TRegisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TUnregisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TWarningActorMessage);
	ActorMessageClassFactory.RegisterMessage(TErrorActorMessage);
	ActorMessageClassFactory.RegisterMessage(TDebugActorMessage);
	ActorMessageClassFactory.RegisterMessage(TInfoActorMessage);
	lLastRequestID := -1;
End;

End.
