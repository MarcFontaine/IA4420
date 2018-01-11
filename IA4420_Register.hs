{-#LANGUAGE GADTs #-}
-- auto-generated
-- do not edit
module IA4420_Register
where
import Data.Word

data Command  where
    ConfigurationSetting :: Command
    PowerManagement :: Command
    FrequencySetting :: Command
    DataRate :: Command
    ReceiverControl :: Command
    DataFilter :: Command
    FifoAndReset :: Command
    ReceiverFifoRead :: Command
    AFC :: Command
    TxConfiguration :: Command
    TransmitterRegisterWrite :: Command
    WakeUpTimerCommand :: Command
    LowDutyCycle :: Command
    LowBatteryDetectorCommand :: Command
    StatusRead :: Command
    deriving (Show,Read,Eq,Ord,Enum,Bounded)

commandPrefix :: Command -> Word16
commandPrefix ConfigurationSetting = 0x8000
commandPrefix PowerManagement = 0x8200
commandPrefix FrequencySetting = 0xa000
commandPrefix DataRate = 0xc600
commandPrefix ReceiverControl = 0x9000
commandPrefix DataFilter = 0xc200
commandPrefix FifoAndReset = 0xca00
commandPrefix ReceiverFifoRead = 0xb000
commandPrefix AFC = 0xc400
commandPrefix TxConfiguration = 0x9800
commandPrefix TransmitterRegisterWrite = 0xb800
commandPrefix WakeUpTimerCommand = 0xe000
commandPrefix LowDutyCycle = 0xc800
commandPrefix LowBatteryDetectorCommand = 0xc000
commandPrefix StatusRead = 0x0

commandPOR :: Command -> Word16
commandPOR ConfigurationSetting = 0x8008
commandPOR PowerManagement = 0x8208
commandPOR FrequencySetting = 0xa680
commandPOR DataRate = 0xc623
commandPOR ReceiverControl = 0x9080
commandPOR DataFilter = 0xc22c
commandPOR FifoAndReset = 0xca80
commandPOR ReceiverFifoRead = 0xb000
commandPOR AFC = 0xc4f7
commandPOR TxConfiguration = 0x9800
commandPOR TransmitterRegisterWrite = 0xb8aa
commandPOR WakeUpTimerCommand = 0xe196
commandPOR LowDutyCycle = 0xc80e
commandPOR LowBatteryDetectorCommand = 0xc000
commandPOR StatusRead = 0x0

data ConfigField  where
    InternalDataRegister :: ConfigField
    FifoMode :: ConfigField
    FrequencyBand :: ConfigField
    CrystalLoadCapacitor :: ConfigField
    ReceiverChain :: ConfigField
    BaseBand :: ConfigField
    Transmisson :: ConfigField
    Synthesizer :: ConfigField
    CrystalOscillator :: ConfigField
    LowBatteryDetector :: ConfigField
    WakeUpTimer :: ConfigField
    DisableClockOutputField :: ConfigField
    Frequency :: ConfigField
    BitRatePrescalar :: ConfigField
    BitRate :: ConfigField
    Pin16Control :: ConfigField
    ValidDataIndicatorResponseTime :: ConfigField
    ReceiverBandWidth :: ConfigField
    LNAGain :: ConfigField
    RSSIThreshold :: ConfigField
    ClockRecoveryAutoLockControl :: ConfigField
    ClockRecoveryLockControl :: ConfigField
    FilterMode :: ConfigField
    DataFilterCommandUnknown :: ConfigField
    FifoITLevel :: ConfigField
    FifoStartCondition :: ConfigField
    FifoFill :: ConfigField
    DisableSensitiveResetField :: ConfigField
    AFCOperationMode :: ConfigField
    AFCMaxDevitation :: ConfigField
    AFCStrobeField :: ConfigField
    AFCFineMode :: ConfigField
    FrequencyOffsetRegister :: ConfigField
    AFCOffsetCalculation :: ConfigField
    FSKPolarity :: ConfigField
    FSKShift :: ConfigField
    RelativeOutputPower :: ConfigField
    TxData :: ConfigField
    WakeUpTimeExponent :: ConfigField
    WakeUpTimeBase :: ConfigField
    DutyCycle :: ConfigField
    LowDutyCyleMode :: ConfigField
    ClockOutputFrequency :: ConfigField
    LowBatteryThresholdVoltage :: ConfigField
    deriving (Show,Read,Eq,Ord)

fieldCommand :: ConfigField -> Command
fieldCommand InternalDataRegister = ConfigurationSetting
fieldCommand FifoMode = ConfigurationSetting
fieldCommand FrequencyBand = ConfigurationSetting
fieldCommand CrystalLoadCapacitor = ConfigurationSetting
fieldCommand ReceiverChain = PowerManagement
fieldCommand BaseBand = PowerManagement
fieldCommand Transmisson = PowerManagement
fieldCommand Synthesizer = PowerManagement
fieldCommand CrystalOscillator = PowerManagement
fieldCommand LowBatteryDetector = PowerManagement
fieldCommand WakeUpTimer = PowerManagement
fieldCommand DisableClockOutputField = PowerManagement
fieldCommand Frequency = FrequencySetting
fieldCommand BitRatePrescalar = DataRate
fieldCommand BitRate = DataRate
fieldCommand Pin16Control = ReceiverControl
fieldCommand ValidDataIndicatorResponseTime = ReceiverControl
fieldCommand ReceiverBandWidth = ReceiverControl
fieldCommand LNAGain = ReceiverControl
fieldCommand RSSIThreshold = ReceiverControl
fieldCommand ClockRecoveryAutoLockControl = DataFilter
fieldCommand ClockRecoveryLockControl = DataFilter
fieldCommand FilterMode = DataFilter
fieldCommand DataFilterCommandUnknown = DataFilter
fieldCommand FifoITLevel = FifoAndReset
fieldCommand FifoStartCondition = FifoAndReset
fieldCommand FifoFill = FifoAndReset
fieldCommand DisableSensitiveResetField = FifoAndReset
fieldCommand AFCOperationMode = AFC
fieldCommand AFCMaxDevitation = AFC
fieldCommand AFCStrobeField = AFC
fieldCommand AFCFineMode = AFC
fieldCommand FrequencyOffsetRegister = AFC
fieldCommand AFCOffsetCalculation = AFC
fieldCommand FSKPolarity = TxConfiguration
fieldCommand FSKShift = TxConfiguration
fieldCommand RelativeOutputPower = TxConfiguration
fieldCommand TxData = TransmitterRegisterWrite
fieldCommand WakeUpTimeExponent = WakeUpTimerCommand
fieldCommand WakeUpTimeBase = WakeUpTimerCommand
fieldCommand DutyCycle = LowDutyCycle
fieldCommand LowDutyCyleMode = LowDutyCycle
fieldCommand ClockOutputFrequency = LowBatteryDetectorCommand
fieldCommand LowBatteryThresholdVoltage = LowBatteryDetectorCommand


fieldOffset :: ConfigField -> Int
fieldOffset InternalDataRegister = 7
fieldOffset FifoMode = 6
fieldOffset FrequencyBand = 4
fieldOffset CrystalLoadCapacitor = 0
fieldOffset ReceiverChain = 7
fieldOffset BaseBand = 6
fieldOffset Transmisson = 5
fieldOffset Synthesizer = 4
fieldOffset CrystalOscillator = 3
fieldOffset LowBatteryDetector = 2
fieldOffset WakeUpTimer = 1
fieldOffset DisableClockOutputField = 0
fieldOffset Frequency = 0
fieldOffset BitRatePrescalar = 7
fieldOffset BitRate = 0
fieldOffset Pin16Control = 10
fieldOffset ValidDataIndicatorResponseTime = 8
fieldOffset ReceiverBandWidth = 5
fieldOffset LNAGain = 3
fieldOffset RSSIThreshold = 0
fieldOffset ClockRecoveryAutoLockControl = 7
fieldOffset ClockRecoveryLockControl = 6
fieldOffset FilterMode = 4
fieldOffset DataFilterCommandUnknown = 0
fieldOffset FifoITLevel = 4
fieldOffset FifoStartCondition = 2
fieldOffset FifoFill = 1
fieldOffset DisableSensitiveResetField = 0
fieldOffset AFCOperationMode = 6
fieldOffset AFCMaxDevitation = 4
fieldOffset AFCStrobeField = 3
fieldOffset AFCFineMode = 2
fieldOffset FrequencyOffsetRegister = 1
fieldOffset AFCOffsetCalculation = 0
fieldOffset FSKPolarity = 8
fieldOffset FSKShift = 4
fieldOffset RelativeOutputPower = 0
fieldOffset TxData = 0
fieldOffset WakeUpTimeExponent = 8
fieldOffset WakeUpTimeBase = 0
fieldOffset DutyCycle = 1
fieldOffset LowDutyCyleMode = 0
fieldOffset ClockOutputFrequency = 5
fieldOffset LowBatteryThresholdVoltage = 0

fieldWidth :: ConfigField -> Int
fieldWidth InternalDataRegister = 1
fieldWidth FifoMode = 1
fieldWidth FrequencyBand = 2
fieldWidth CrystalLoadCapacitor = 4
fieldWidth ReceiverChain = 1
fieldWidth BaseBand = 1
fieldWidth Transmisson = 1
fieldWidth Synthesizer = 1
fieldWidth CrystalOscillator = 1
fieldWidth LowBatteryDetector = 1
fieldWidth WakeUpTimer = 1
fieldWidth DisableClockOutputField = 1
fieldWidth Frequency = 12
fieldWidth BitRatePrescalar = 1
fieldWidth BitRate = 7
fieldWidth Pin16Control = 1
fieldWidth ValidDataIndicatorResponseTime = 2
fieldWidth ReceiverBandWidth = 3
fieldWidth LNAGain = 2
fieldWidth RSSIThreshold = 3
fieldWidth ClockRecoveryAutoLockControl = 1
fieldWidth ClockRecoveryLockControl = 1
fieldWidth FilterMode = 1
fieldWidth DataFilterCommandUnknown = 3
fieldWidth FifoITLevel = 4
fieldWidth FifoStartCondition = 1
fieldWidth FifoFill = 1
fieldWidth DisableSensitiveResetField = 1
fieldWidth AFCOperationMode = 2
fieldWidth AFCMaxDevitation = 2
fieldWidth AFCStrobeField = 1
fieldWidth AFCFineMode = 1
fieldWidth FrequencyOffsetRegister = 1
fieldWidth AFCOffsetCalculation = 1
fieldWidth FSKPolarity = 1
fieldWidth FSKShift = 4
fieldWidth RelativeOutputPower = 4
fieldWidth TxData = 8
fieldWidth WakeUpTimeExponent = 5
fieldWidth WakeUpTimeBase = 8
fieldWidth DutyCycle = 7
fieldWidth LowDutyCyleMode = 1
fieldWidth ClockOutputFrequency = 3
fieldWidth LowBatteryThresholdVoltage = 5

data ConfigConst  where
    EnableInternalDataRegister :: ConfigConst
    DisableInternalDataRegister :: ConfigConst
    EnableFifoMode :: ConfigConst
    DisableFifoMode :: ConfigConst
    FrequencyBand315 :: ConfigConst
    FrequencyBand433 :: ConfigConst
    FrequencyBand868 :: ConfigConst
    FrequencyBand915 :: ConfigConst
    EnableReceiverChain :: ConfigConst
    DisableReceiverChain :: ConfigConst
    EnableBaseBand :: ConfigConst
    DisableBaseBand :: ConfigConst
    EnableTransmisson :: ConfigConst
    DisableTransmisson :: ConfigConst
    EnableSynthesizer :: ConfigConst
    DisableSynthesizer :: ConfigConst
    EnableCrystalOscillator :: ConfigConst
    DisableCrystalOscillator :: ConfigConst
    EnableLowBatteryDetector :: ConfigConst
    DisableLowBatteryDetector :: ConfigConst
    EnableWakeUpTimer :: ConfigConst
    DisableWakeUpTimer :: ConfigConst
    DisableClockOutput :: ConfigConst
    EnableClockOutput :: ConfigConst
    EnableBitRatePrescalar :: ConfigConst
    DisableBitRatePrescalar :: ConfigConst
    EnablePin16Control :: ConfigConst
    DisablePin16Control :: ConfigConst
    VDIFast :: ConfigConst
    VDIMedium :: ConfigConst
    VDISlow :: ConfigConst
    VDIAlwaysOn :: ConfigConst
    RBW_400 :: ConfigConst
    RBW_340 :: ConfigConst
    RBW_270 :: ConfigConst
    RBW_200 :: ConfigConst
    RBW_134 :: ConfigConst
    RBW_67 :: ConfigConst
    LNA_0dB :: ConfigConst
    LNA_6dB :: ConfigConst
    LNA_14dB :: ConfigConst
    LNA_20dB :: ConfigConst
    RSSI_103dBm :: ConfigConst
    RSSI_97dBm :: ConfigConst
    RSSI_91dBm :: ConfigConst
    RSSI_85dBm :: ConfigConst
    RSSI_79dBm :: ConfigConst
    RSSI_73dBm :: ConfigConst
    EnableClockRecoveryAutoLockControl :: ConfigConst
    DisableClockRecoveryAutoLockControl :: ConfigConst
    EnableClockRecoveryLockControl :: ConfigConst
    DisableClockRecoveryLockControl :: ConfigConst
    EnableFilterMode :: ConfigConst
    DisableFilterMode :: ConfigConst
    EnableFifoStartCondition :: ConfigConst
    DisableFifoStartCondition :: ConfigConst
    EnableFifoFill :: ConfigConst
    DisableFifoFill :: ConfigConst
    DisableSensitiveReset :: ConfigConst
    EnableSensitiveReset :: ConfigConst
    AFCAutoOff :: ConfigConst
    AFCRunOnce :: ConfigConst
    AFCKeepOffsetOnlyReceiving :: ConfigConst
    AFCKeepOffset :: ConfigConst
    AFCDeviationUnrestricted :: ConfigConst
    AFCDeviation_15 :: ConfigConst
    AFCDeviation_7 :: ConfigConst
    AFCDeviation_3 :: ConfigConst
    AFCStrobe :: ConfigConst
    EnableAFCFineMode :: ConfigConst
    DisableAFCFineMode :: ConfigConst
    EnableFrequencyOffsetRegister :: ConfigConst
    DisableFrequencyOffsetRegister :: ConfigConst
    EnableAFCOffsetCalculation :: ConfigConst
    DisableAFCOffsetCalculation :: ConfigConst
    FSKPolarityNormal :: ConfigConst
    FSKPolarityReverse :: ConfigConst
    EnableLowDutyCyleMode :: ConfigConst
    DisableLowDutyCyleMode :: ConfigConst
    ClockOut_1000kHz :: ConfigConst
    ClockOut_1250kHz :: ConfigConst
    ClockOut_1660kHz :: ConfigConst
    ClockOut_2000kHz :: ConfigConst
    ClockOut_2500kHz :: ConfigConst
    ClockOut_3330kHz :: ConfigConst
    ClockOut_5000kHz :: ConfigConst
    ClockOut_10000kHz :: ConfigConst
    deriving (Show,Read,Eq,Ord)

configConstField :: ConfigConst -> ConfigField
configConstField EnableInternalDataRegister = InternalDataRegister
configConstField DisableInternalDataRegister = InternalDataRegister
configConstField EnableFifoMode = FifoMode
configConstField DisableFifoMode = FifoMode
configConstField FrequencyBand315 = FrequencyBand
configConstField FrequencyBand433 = FrequencyBand
configConstField FrequencyBand868 = FrequencyBand
configConstField FrequencyBand915 = FrequencyBand
configConstField EnableReceiverChain = ReceiverChain
configConstField DisableReceiverChain = ReceiverChain
configConstField EnableBaseBand = BaseBand
configConstField DisableBaseBand = BaseBand
configConstField EnableTransmisson = Transmisson
configConstField DisableTransmisson = Transmisson
configConstField EnableSynthesizer = Synthesizer
configConstField DisableSynthesizer = Synthesizer
configConstField EnableCrystalOscillator = CrystalOscillator
configConstField DisableCrystalOscillator = CrystalOscillator
configConstField EnableLowBatteryDetector = LowBatteryDetector
configConstField DisableLowBatteryDetector = LowBatteryDetector
configConstField EnableWakeUpTimer = WakeUpTimer
configConstField DisableWakeUpTimer = WakeUpTimer
configConstField DisableClockOutput = DisableClockOutputField
configConstField EnableClockOutput = DisableClockOutputField
configConstField EnableBitRatePrescalar = BitRatePrescalar
configConstField DisableBitRatePrescalar = BitRatePrescalar
configConstField EnablePin16Control = Pin16Control
configConstField DisablePin16Control = Pin16Control
configConstField VDIFast = ValidDataIndicatorResponseTime
configConstField VDIMedium = ValidDataIndicatorResponseTime
configConstField VDISlow = ValidDataIndicatorResponseTime
configConstField VDIAlwaysOn = ValidDataIndicatorResponseTime
configConstField RBW_400 = ReceiverBandWidth
configConstField RBW_340 = ReceiverBandWidth
configConstField RBW_270 = ReceiverBandWidth
configConstField RBW_200 = ReceiverBandWidth
configConstField RBW_134 = ReceiverBandWidth
configConstField RBW_67 = ReceiverBandWidth
configConstField LNA_0dB = LNAGain
configConstField LNA_6dB = LNAGain
configConstField LNA_14dB = LNAGain
configConstField LNA_20dB = LNAGain
configConstField RSSI_103dBm = RSSIThreshold
configConstField RSSI_97dBm = RSSIThreshold
configConstField RSSI_91dBm = RSSIThreshold
configConstField RSSI_85dBm = RSSIThreshold
configConstField RSSI_79dBm = RSSIThreshold
configConstField RSSI_73dBm = RSSIThreshold
configConstField EnableClockRecoveryAutoLockControl = ClockRecoveryAutoLockControl
configConstField DisableClockRecoveryAutoLockControl = ClockRecoveryAutoLockControl
configConstField EnableClockRecoveryLockControl = ClockRecoveryLockControl
configConstField DisableClockRecoveryLockControl = ClockRecoveryLockControl
configConstField EnableFilterMode = FilterMode
configConstField DisableFilterMode = FilterMode
configConstField EnableFifoStartCondition = FifoStartCondition
configConstField DisableFifoStartCondition = FifoStartCondition
configConstField EnableFifoFill = FifoFill
configConstField DisableFifoFill = FifoFill
configConstField DisableSensitiveReset = DisableSensitiveResetField
configConstField EnableSensitiveReset = DisableSensitiveResetField
configConstField AFCAutoOff = AFCOperationMode
configConstField AFCRunOnce = AFCOperationMode
configConstField AFCKeepOffsetOnlyReceiving = AFCOperationMode
configConstField AFCKeepOffset = AFCOperationMode
configConstField AFCDeviationUnrestricted = AFCMaxDevitation
configConstField AFCDeviation_15 = AFCMaxDevitation
configConstField AFCDeviation_7 = AFCMaxDevitation
configConstField AFCDeviation_3 = AFCMaxDevitation
configConstField AFCStrobe = AFCStrobeField
configConstField EnableAFCFineMode = AFCFineMode
configConstField DisableAFCFineMode = AFCFineMode
configConstField EnableFrequencyOffsetRegister = FrequencyOffsetRegister
configConstField DisableFrequencyOffsetRegister = FrequencyOffsetRegister
configConstField EnableAFCOffsetCalculation = AFCOffsetCalculation
configConstField DisableAFCOffsetCalculation = AFCOffsetCalculation
configConstField FSKPolarityNormal = FSKPolarity
configConstField FSKPolarityReverse = FSKPolarity
configConstField EnableLowDutyCyleMode = LowDutyCyleMode
configConstField DisableLowDutyCyleMode = LowDutyCyleMode
configConstField ClockOut_1000kHz = ClockOutputFrequency
configConstField ClockOut_1250kHz = ClockOutputFrequency
configConstField ClockOut_1660kHz = ClockOutputFrequency
configConstField ClockOut_2000kHz = ClockOutputFrequency
configConstField ClockOut_2500kHz = ClockOutputFrequency
configConstField ClockOut_3330kHz = ClockOutputFrequency
configConstField ClockOut_5000kHz = ClockOutputFrequency
configConstField ClockOut_10000kHz = ClockOutputFrequency

configConstValue :: ConfigConst -> Word16
configConstValue EnableInternalDataRegister = 1
configConstValue DisableInternalDataRegister = 0
configConstValue EnableFifoMode = 1
configConstValue DisableFifoMode = 0
configConstValue FrequencyBand315 = 0
configConstValue FrequencyBand433 = 1
configConstValue FrequencyBand868 = 2
configConstValue FrequencyBand915 = 3
configConstValue EnableReceiverChain = 1
configConstValue DisableReceiverChain = 0
configConstValue EnableBaseBand = 1
configConstValue DisableBaseBand = 0
configConstValue EnableTransmisson = 1
configConstValue DisableTransmisson = 0
configConstValue EnableSynthesizer = 1
configConstValue DisableSynthesizer = 0
configConstValue EnableCrystalOscillator = 1
configConstValue DisableCrystalOscillator = 0
configConstValue EnableLowBatteryDetector = 1
configConstValue DisableLowBatteryDetector = 0
configConstValue EnableWakeUpTimer = 1
configConstValue DisableWakeUpTimer = 0
configConstValue DisableClockOutput = 1
configConstValue EnableClockOutput = 0
configConstValue EnableBitRatePrescalar = 1
configConstValue DisableBitRatePrescalar = 0
configConstValue EnablePin16Control = 1
configConstValue DisablePin16Control = 0
configConstValue VDIFast = 0
configConstValue VDIMedium = 1
configConstValue VDISlow = 2
configConstValue VDIAlwaysOn = 3
configConstValue RBW_400 = 1
configConstValue RBW_340 = 2
configConstValue RBW_270 = 3
configConstValue RBW_200 = 4
configConstValue RBW_134 = 5
configConstValue RBW_67 = 6
configConstValue LNA_0dB = 0
configConstValue LNA_6dB = 1
configConstValue LNA_14dB = 2
configConstValue LNA_20dB = 3
configConstValue RSSI_103dBm = 0
configConstValue RSSI_97dBm = 1
configConstValue RSSI_91dBm = 2
configConstValue RSSI_85dBm = 3
configConstValue RSSI_79dBm = 4
configConstValue RSSI_73dBm = 5
configConstValue EnableClockRecoveryAutoLockControl = 1
configConstValue DisableClockRecoveryAutoLockControl = 0
configConstValue EnableClockRecoveryLockControl = 1
configConstValue DisableClockRecoveryLockControl = 0
configConstValue EnableFilterMode = 1
configConstValue DisableFilterMode = 0
configConstValue EnableFifoStartCondition = 1
configConstValue DisableFifoStartCondition = 0
configConstValue EnableFifoFill = 1
configConstValue DisableFifoFill = 0
configConstValue DisableSensitiveReset = 1
configConstValue EnableSensitiveReset = 0
configConstValue AFCAutoOff = 0
configConstValue AFCRunOnce = 1
configConstValue AFCKeepOffsetOnlyReceiving = 2
configConstValue AFCKeepOffset = 3
configConstValue AFCDeviationUnrestricted = 0
configConstValue AFCDeviation_15 = 1
configConstValue AFCDeviation_7 = 2
configConstValue AFCDeviation_3 = 3
configConstValue AFCStrobe = 1
configConstValue EnableAFCFineMode = 1
configConstValue DisableAFCFineMode = 0
configConstValue EnableFrequencyOffsetRegister = 1
configConstValue DisableFrequencyOffsetRegister = 0
configConstValue EnableAFCOffsetCalculation = 1
configConstValue DisableAFCOffsetCalculation = 0
configConstValue FSKPolarityNormal = 1
configConstValue FSKPolarityReverse = 0
configConstValue EnableLowDutyCyleMode = 1
configConstValue DisableLowDutyCyleMode = 0
configConstValue ClockOut_1000kHz = 0
configConstValue ClockOut_1250kHz = 1
configConstValue ClockOut_1660kHz = 2
configConstValue ClockOut_2000kHz = 3
configConstValue ClockOut_2500kHz = 4
configConstValue ClockOut_3330kHz = 5
configConstValue ClockOut_5000kHz = 6
configConstValue ClockOut_10000kHz = 7
