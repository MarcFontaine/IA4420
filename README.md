# IA4420 / RFM12B

Some Haskell scripts for experimenting with the IA4420 FSK transceiver IC.
The IA4420 is the IC that is found on RFM12B transceiver modules.

Typical application are:

* Home security and alarm

* Door bells

* Remote control, key-less entry (garage doors)

* Telemetry

There are modules available for 433MHz , 868MHz and 915MHz bands
(a single module works only on one band).
They communicate with the host via a SPI interface.

The repository contains a mixture of generic library code
and code for some RFM12B-experiments.
The Haskell scripts provide:

* Human readable names for registers and flags of the IC
(taken from the data sheet).

* SPI-communication via the STM32-Zombie library / hardware.

* The TRX monad.

* Persisting the IC-state to a file (for GHCi / REPL / module reloads).

* Real-time high-speed DMA.


