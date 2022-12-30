#builder

(source: )

**Builders are used to efficiently construct sequences of bytes from smaller parts.** Typically, such a construction is part of the implementation of an encoding, i.e., a function for converting Haskell values to sequences of bytes. Examples of encodings are the generation of the sequence of bytes representing a HTML document to be sent in a HTTP response by a web application or the serialization of a Haskell value using a fixed binary format.

**Internally, Builders are buffer-filling functions.** They are executed by a driver that provides them with an actual buffer to fill. Once called with a buffer, a Builder fills it and returns a signal to the driver telling it that it is either done, has filled the current buffer, or wants to directly insert a reference to a chunk of memory. In the last two cases, the Builder also returns a continuation Builder that the driver can call to fill the next buffer. Here, we provide the two drivers that satisfy almost all use cases. See `Data.ByteString.Builder.Extra`, for information about fine-tuning them.