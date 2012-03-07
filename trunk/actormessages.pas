{
  This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
  the Free Software Foundation; version 2 of the License.
   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
}

// Copyright (c) 2010 2011 - J. Aldo G. de Freitas Junior

{$MODE DELPHI}{$M+}{$H+}

Unit
	ActorMessages;

Interface

Uses
	Classes,
	SysUtils,
	RTTIObjects;

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
	Public
		Constructor Create(Const aSource, aDestination : String); Virtual;
		Destructor Destroy; Override;
		Function Clone : TCustomActorMessage; Virtual;
	Published
		Property Source : String Read fSource Write fSource;
		Property Destination : String Read fDestination Write fDestination;
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
		fClasses : Array Of TCustomActorMessageClass;
	Public
		Procedure RegisterMessage(Const aClass : TCustomActorMessageClass);
		Function Build(Const aClassName : String): TCustomActorMessage;
	End;

Var
	ActorMessageClassFactory : TActorMessageClassFactory;

Implementation

// TCustomActorMessage

Constructor TCustomActorMessage.Create(Const aSource, aDestination : String);
Begin
	Inherited Create;
	fSource := aSource;
	fDestination := aDestination;
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

// TActorMessageClassFactory

Procedure TActorMessageClassFactory.RegisterMessage(Const aClass : TCustomActorMessageClass);
Begin
	SetLength(fClasses, Length(fClasses) + 1);
	fClasses[High(fClasses)] := aClass;
End;

Function TActorMessageClassFactory.Build(Const aClassName : String): TCustomActorMessage;
Var
	lCtrl : Integer;
Begin
	Result := Nil;
	For lCtrl := Low(fClasses) To High(fClasses) Do
		If LowerCase(fClasses[lCtrl].ClassName) = LowerCase(aClassName) Then
		Begin
			Result := TCustomActorMessageClass(fClasses[lCtrl]).Create('', '');
			Exit;
		End;
End;

Initialization

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
	ActorMessageClassFactory.RegisterMessage(TConfigInstanceActorMessage);
	ActorMessageClassFactory.RegisterMessage(TRegisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TUnregisterClassActorMessage);
	ActorMessageClassFactory.RegisterMessage(TCustomLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TLogActorMessage);
	ActorMessageClassFactory.RegisterMessage(TWarningActorMessage);
	ActorMessageClassFactory.RegisterMessage(TErrorActorMessage);
	ActorMessageClassFactory.RegisterMessage(TDebugActorMessage);
	ActorMessageClassFactory.RegisterMessage(TInfoActorMessage);

Finalization

	ActorMessageClassFactory.Free;

End.
