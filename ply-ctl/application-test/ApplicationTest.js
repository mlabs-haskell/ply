exports.stdoutWrite = (str) => {
    var write = process.stdout.write;
    return () => write.call(process.stdout, str);
};
