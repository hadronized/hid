-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module System.HID (
    -- * Initialization and finalization
    init
  , exit
    -- * Getting devices information
  , DeviceInfo
      (
        devPath
      , devVendorID
      , devProductID
      , devSerialNumber
      , devReleaseNumber
      , devManufacturerString
      , devProductString
      , devUsagePage
      , devUsage
      , devInterfaceNumber
      )
  , enumerate
  , detectDevices
    -- * Accessing devices
  , Device
  , vendorProductSerialDevice
  , pathDevice
    -- * Sending data to devices
  , writeOutputReport
  , sendFeatureReport
    -- * Receiving data from devices
  , readInputReport
  , readInputReportTimeout
  , getFeatureReport
  , getManufacturer
  , getProductName
  , getSerialNumber
  , getIndexedString
    -- * Blocking mode for devices
  , setBlocking
    -- * Getting errors
  , getError
  ) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Word
import Foreign.Concurrent
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr )
import Foreign.C.String
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding ( init, product )
import System.HID.Internal.Functions
import System.HID.Internal.Types
import System.HID.Internal.Utils

maxReadBytes :: Int 
maxReadBytes = 524288

-- |Initialize the library.
--
-- This function initializes the library. Calling it is not strictly
-- necessary, as it will be called automatically when enumerating or opening
-- devices if it’s needed. This function should be called at the beginning of
-- execution however, if there is a chance of handles being opened by different
-- threads simultaneously.
init :: (MonadIO m) => m Bool
init = liftIO $ fmap fromHIDRet hidInit

-- |Finalize the library.
--
-- This function frees all of the static data associated with the library. It
-- should be called at the end of execution to avoid memory leaks.
exit :: (MonadIO m) => m Bool
exit = liftIO $ fmap fromHIDRet hidExit

-- |Information on a device.
data DeviceInfo = DeviceInfo {
    devPath               :: String
  , devVendorID           :: Word16
  , devProductID          :: Word16
  , devSerialNumber       :: String
  , devReleaseNumber      :: Word16
  , devManufacturerString :: String
  , devProductString      :: String
  , devUsagePage          :: Word16
  , devUsage              :: Word16
  , devInterfaceNumber    :: Int
  } deriving (Eq,Show)

-- |Enumerate all devices for a given vendor ID and product ID. If you need to
-- get all available devices, use 'detectDevices'.
enumerate :: (MonadIO m) => Word32 -> Word32 -> m [DeviceInfo]
enumerate vendorID productID = do
  devs <- liftIO (hidEnumerate (fromIntegral vendorID) (fromIntegral productID)) >>= scanDevs
  traverse extractDeviceInfo devs
  
-- |Enumerate all plugged in devices.
detectDevices :: (MonadIO m) => m [DeviceInfo]
detectDevices = enumerate 0 0

-- |Extract a 'DeviceInfo' out of a pointer on a 'HIDDeviceInfo'.
extractDeviceInfo :: (MonadIO m) => Ptr HIDDeviceInfo -> m DeviceInfo
extractDeviceInfo p = liftIO $ do
  HIDDeviceInfo cpath cvid cpid cserial crelease cmanu cproduct cusagep cusage cifnb _ <- peek p
  path <- if cpath == nullPtr then pure "" else peekCString cpath
  serial <- if cserial == nullPtr then pure "" else peekCWString cserial
  manu <- if cmanu == nullPtr then pure "" else peekCWString cmanu
  product <- if cproduct == nullPtr then pure "" else peekCWString cproduct
  pure $ DeviceInfo path (fromIntegral cvid)
    (fromIntegral cpid) serial
    (fromIntegral crelease) manu product
    (fromIntegral cusagep) (fromIntegral cusage)
    (fromIntegral cifnb)

-- |Scan the list of devices.
scanDevs :: (MonadIO m) => Ptr HIDDeviceInfo -> m [Ptr HIDDeviceInfo]
scanDevs hidDeviceInfo
  | hidDeviceInfo == nullPtr = pure []
  | otherwise =
      fmap (hidDeviceInfo :) $ liftIO (peek hidDeviceInfo) >>= scanDevs . hidNext

-- |An opaque device.
newtype Device = Device { unDevice :: ForeignPtr HIDDevice }

-- |Get a 'Device' from the vendor ID, product ID and an optional serial number.
vendorProductSerialDevice :: (MonadIO m) => Word16 -> Word16 -> Maybe String -> m (Maybe Device)
vendorProductSerialDevice vendorID productID optSerial =
  liftIO $ case optSerial of
    Just serial -> do
      withCWString serial $ \cserial -> do
        hidDev <- hidOpen (fromIntegral vendorID) (fromIntegral productID) cserial
        if hidDev == nullPtr then pure Nothing else fmap Just $ wrapHIDDevice hidDev
    Nothing -> do
        hidDev <- hidOpen (fromIntegral vendorID) (fromIntegral productID) nullPtr 
        if hidDev == nullPtr then pure Nothing else fmap Just $ wrapHIDDevice hidDev

-- |Get a 'Device' from a path.
pathDevice :: (MonadIO m) => String -> m (Maybe Device)
pathDevice path = do
  liftIO . withCString path $ \cpath -> do
    hidDev <- hidOpenPath cpath
    if hidDev == nullPtr then pure Nothing else fmap Just $ wrapHIDDevice hidDev

-- |Wrap a 'HIDDevice' into a 'Device'. Calls 'hidClose' when unused via
-- garbage collection.
wrapHIDDevice :: (MonadIO m) => Ptr HIDDevice -> m Device
wrapHIDDevice p = liftIO . fmap Device $ newForeignPtr p (hidClose p)

-- |Write an Output report to a HID device.
--
-- The first byte of data must contain the Report ID. For devices which only
-- support a single report, this must be set to 0x0. The remaining bytes contain
-- the report data. Since the Report ID is mandatory, calls to
-- 'writeOutputReport' will always contain one more byte(s) than the report
-- contains. For example, if a hid report is 16 bytes long, 17 bytes must be
-- passed to 'writeOutputReport', the Report ID (or 0x0, for devices with a
-- single report), followed by the report data (16 bytes). In that example, the
-- length passed in would be 17.
--
-- 'writeOutputReport' will send the data on the first OUT endpoint, if one
-- exists. If it does not, it will send the data through the Control Endpoint
-- Endpoint 0).
writeOutputReport :: (MonadIO m) => Device -> BS.ByteString -> m Int
writeOutputReport dev bs = liftIO . fmap fromIntegral $
  withForeignPtr (unDevice dev) $ \hidDev ->
    withArrayLen (BS.unpack bs) $ \bytes cdata ->
      hidWrite hidDev (castPtr cdata) (fromIntegral bytes)

-- |Send a Feature report to the device.
--
-- Feature reports are sent over the Control endpoint as a Set_Report transfer.
-- The first byte of data must contain the Report ID. For devices which only
-- upport a single report, this must be set to 0x0. The remaining bytes contain
-- the report data. Since the Report ID is mandatory, calls to
-- 'sendFeatureReport' will always contain one more byte than the report
-- contains. For example, if a hid report is 16 bytes long, 17 bytes must be
-- passed to 'sendFeatureReport': the Report ID (or 0x0, for devices which do
-- not use numbered reports), followed by the report data (16 bytes). In that
-- example, the length passed in would be 17.
sendFeatureReport :: (MonadIO m) => Device -> BS.ByteString -> m Int
sendFeatureReport dev bs = liftIO . fmap fromIntegral $
  withForeignPtr (unDevice dev) $ \hidDev ->
    withArrayLen (BS.unpack bs) $ \bytes cdata ->
      hidSendFeatureReport hidDev (castPtr cdata) (fromIntegral bytes)

-- |Read an Input report from a HID device.
--
-- Input reports are returned to the host through the INTERRUPT IN endpoint.
-- The first byte will contain the Report number if the device uses numbered
-- reports.
readInputReport :: (MonadIO m) => Device -> m (Maybe BS.ByteString)
readInputReport dev = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    allocaBytes maxReadBytes $ \inputData -> do
      r <- hidRead hidDev inputData (fromIntegral maxReadBytes)
      if r < 0 then
        pure Nothing
        else do
          bs <- fmap (BS.pack) $ peekArray (fromIntegral r) (castPtr inputData)
          pure $ Just bs

-- |Read an Input report from a HID device with timeout.
--
-- Input reports are returned to the host through the INTERRUPT IN endpoint.
-- The first byte will contain the Report number if the device uses numbered
-- reports.
readInputReportTimeout :: (MonadIO m) => Device -> Int -> m (Maybe BS.ByteString)
readInputReportTimeout dev tms = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    allocaBytes maxReadBytes $ \inputData -> do
      r <- hidReadTimeout hidDev inputData (fromIntegral maxReadBytes) (fromIntegral tms)
      if r < 0 then
        pure Nothing
        else do
          bs <- fmap (BS.pack) $ peekArray (fromIntegral r) (castPtr inputData)
          pure $ Just bs

-- |Get a feature report from a HID device.
--
-- Set the first byte of data to the Report ID of the report to be read. Make
-- sure to allow space for this extra byte in data. Upon return, the first byte
-- will still contain the Report ID, and the report data will start in data[1].
getFeatureReport :: (MonadIO m) => Device -> m (Maybe BS.ByteString)
getFeatureReport dev = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    allocaBytes maxReadBytes $ \inputData -> do
      r <- hidGetFeatureReport hidDev inputData (fromIntegral maxReadBytes)
      if r < 0 then
        pure Nothing
        else do
          bs <- fmap (BS.pack) $ peekArray (fromIntegral r) (castPtr inputData)
          pure $ Just bs

-- |Get the manufacturer string from a HID device. 
getManufacturer :: (MonadIO m) => Device -> m String
getManufacturer dev = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    withCWString (replicate maxReadBytes '\0') $ \inputData -> do
      _ <- hidGetManufacturerString hidDev inputData (fromIntegral maxReadBytes)
      peekCWString inputData

-- |Get the product name from a HID device. 
getProductName :: (MonadIO m) => Device -> m String
getProductName dev = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    withCWString (replicate maxReadBytes '\0') $ \inputData -> do
      _ <- hidGetProductString hidDev inputData (fromIntegral maxReadBytes)
      peekCWString inputData

-- |Get the serial number string from a HID device. 
getSerialNumber :: (MonadIO m) => Device -> m String
getSerialNumber dev = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    withCWString (replicate maxReadBytes '\0') $ \inputData -> do
      _ <- hidGetSerialNumberString hidDev inputData (fromIntegral maxReadBytes)
      peekCWString inputData

-- |Get an indexed string from a HID device. 
getIndexedString :: (MonadIO m) => Device -> Int -> m String
getIndexedString dev index = liftIO $ do
  withForeignPtr (unDevice dev) $ \hidDev ->
    withCWString (replicate maxReadBytes '\0') $ \inputData -> do
      _ <- hidGetIndexedString hidDev (fromIntegral index) inputData (fromIntegral maxReadBytes)
      peekCWString inputData

-- |Set the blocking mode of a device.
setBlocking :: (MonadIO m) => Device -> Bool -> m Bool
setBlocking dev blocking = liftIO $
  withForeignPtr (unDevice dev) $ \hidDev ->
    fmap fromHIDRet $ hidSetNonblocking hidDev (fromBool $ not blocking)

-- |Get last error.
getError :: (MonadIO m) => Device -> m String
getError dev = liftIO $ withForeignPtr (unDevice dev) $ \hidDev ->
  hidError hidDev >>= peekCWString
