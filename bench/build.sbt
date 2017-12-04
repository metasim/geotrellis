enablePlugins(GTBenchmarkPlugin)

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

jmhIterations := Some(20)
jmhWarmupIterations := Some(5)
jmhTimeUnit := Some("ms")
jmhExtraOptions := Some("-jvmArgsAppend -Xmx2G -prof geotrellis.bench.GeotrellisFlightRecordingProfiler")
