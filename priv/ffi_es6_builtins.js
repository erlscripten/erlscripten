"use strict";

var RUNTIME = function(){

var pid_ctr = 0;
var reference_ctr = 0;

const process_state = {
    NORMAL: Symbol.for('normal'),
    KILL: Symbol.for('kill'),
    SUSPEND: Symbol.for('suspend'),
    CONTINUE: Symbol.for('continue'),
    RECEIVE: Symbol.for('receive'),
    SEND: Symbol.for('send'),
    SLEEPING: Symbol.for('sleeping'),
    RUNNING: Symbol.for('running'),
    SUSPENDED: Symbol.for('suspended'),
    STOPPED: Symbol.for('stopped'),
    SLEEP: Symbol.for('sleep'),
    EXIT: Symbol.for('exit'),
    NOMATCH: Symbol.for('no_match'),
};

class PID {
    constructor() {
        this.id = pid_ctr;
        pid_ctr = pid_ctr + 1;
    }
}

class REF {
    constructor() {
        this.id = reference_ctr;
        reference_ctr = reference_ctr + 1;
    }
}

class ProcessQueue {
    constructor(pid) {
        this.pid = pid;
        this.tasks = [];
    }
    empty() {
        return this.tasks.length === 0;
    }
    add(task) {
        this.tasks.push(task);
    }
    next() {
        return this.tasks.shift();
    }
}

/**
 * Default scheduler for the process system.
 * Schedules process execution using setTimeout.
 * The most generic scheduler and maybe not good for
 * anything with dom manipulation.
 */
class DefaultScheduler {
    constructor(throttle = 0, reductions_per_process = 8) {
        this.isRunning = false;
        this.invokeLater = function (callback) {
            setTimeout(callback, throttle);
        };
        // In our case a reduction is equal to a task call
        // Controls how many tasks are called at a time per process
        this.reductions_per_process = reductions_per_process;
        this.queues = new Map();
        console.log("SCHEDULER")
        console.log(this.isRunning)
        this.run();
    }
    addToQueue(pid, task) {
        if (!this.queues.has(pid)) {
            this.queues.set(pid, new ProcessQueue(pid));
        }
        const queue = this.queues.get(pid);
        if (queue) {
            queue.add(task);
        }
    }
    removePid(pid) {
        this.isRunning = true;
        this.queues.delete(pid);
        this.isRunning = false;
    }
    _run(run) {
        console.log("_run()");
        this.invokeLater(() => {
            run();
        });
    }
    run() {
        console.log("run()");
        if (this.isRunning) {
            this._run(this.run.bind(this));
        }
        else {
            for (let [pid, queue] of this.queues) {
                let reductions = 0;
                while (queue &&
                    !queue.empty() &&
                    reductions < this.reductions_per_process) {
                    let task = queue.next();
                    this.isRunning = true;
                    let result;
                    try {
                        if (task) {
                            result = task();
                        }
                    }
                    catch (e) {
                        console.error(e);
                        result = e;
                    }
                    this.isRunning = false;
                    if (result instanceof Error) {
                        throw result;
                    }
                    reductions++;
                }
            }
            this._run(this.run.bind(this));
        }
    }
    addToScheduler(pid, task, dueTime = 0) {
        if (dueTime === 0) {
            this.invokeLater(() => {
                this.addToQueue(pid, task);
            });
        }
        else {
            setTimeout(() => {
                this.addToQueue(pid, task);
            }, dueTime);
        }
    }
    schedule(pid, task) {
        this.addToScheduler(pid, () => {
            task();
        });
    }
    scheduleFuture(pid, dueTime, task) {
        this.addToScheduler(pid, () => {
            task();
        }, dueTime);
    }
}

/**
 * Scheduler for the process system.
 * Uses window.requestAnimationFrame to schedule process execution
 * Good for processes that do a lot of dom manipulation
 */
class RequestAnimationScheduler extends DefaultScheduler {
    constructor(throttle = 0, reductions_per_process = 8) {
        super(throttle, reductions_per_process);
    }
    _run(run) {
        window.requestAnimationFrame(run);
    }
}

/**
 * Manages a process's messages.
 * A message is anything sent to the process from another
 * process
 */
class Mailbox {
    constructor() {
        this.messages = [];
    }
    deliver(message) {
        this.messages.push(message);
        return message;
    }
    get() {
        return this.messages;
    }
    isEmpty() {
        return this.messages.length === 0;
    }
    removeAt(index) {
        this.messages.splice(index, 1);
    }
}

function is_sleep(value) {
    return Array.isArray(value) && value[0] === process_state.SLEEP;
}
function is_receive(value) {
    return Array.isArray(value) && value[0] === process_state.RECEIVE;
}
function receive_timed_out(value) {
    return value[2] != null && value[2] < Date.now();
}
/**
 * A Process. Represents the basic atomic level of concurrency in the system
 */
class Process {
    constructor(system, func, args) {
        // TODO: Add group leader OwO
        this.system = system;
        this.func = func;
        this.args = args;
        this.status = process_state.STOPPED;
        this.pid = new PID();
        this.mailbox = new Mailbox();
        this.dict = new Map();
        this.flags = new Map();
        this.monitors = [];
    }
    start() {
        console.log("Process start")
        const function_scope = this;
        let machine = this.main();
        this.system.schedule(function () {
            function_scope.system.set_current(function_scope.pid);
            function_scope.run(machine, machine.next());
        }, this.pid);
    }
    *main() {
        let retval = process_state.NORMAL;
        try {
            yield* this.func.apply(null, this.args);
        }
        catch (e) {
            console.error(e);
            retval = e;
        }
        this.system.exit(retval);
    }
    process_flag(flag, value) {
        const old_value = this.flags.get(flag);
        this.flags.set(flag, value);
        return old_value;
    }
    is_trapping_exits() {
        return (this.flags.has(Symbol.for('trap_exit')) &&
            this.flags.get(Symbol.for('trap_exit')) == true);
    }
    signal(reason) {
        if (reason !== process_state.NORMAL) {
            console.error(reason);
        }
        this.system.remove_proc(this.pid, reason);
    }
    receive(fun) {
        console.log("RECEIVE")
        let value = process_state.NOMATCH;
        let messages = this.mailbox.get();
        for (let i = 0; i < messages.length; i++) {
            try {
                value = fun(messages[i]);
                if (value !== process_state.NOMATCH) {
                    this.mailbox.removeAt(i);
                    break;
                }
            }
            catch (e) {
                if (e.constructor.name != 'MatchError') {
                    this.system.exit(e);
                }
            }
        }
        return value;
    }
    run(machine, step) {
        console.log("Process run")
        const function_scope = this;
        if (!step.done) {
            let value = step.value;
            if (is_sleep(value)) {
                this.system.delay(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next());
                }, value[1]);
            }
            else if (is_receive(value) && receive_timed_out(value)) {
                let result = value[3]();
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(result));
                });
            }
            else if (is_receive(value)) {
                let result = function_scope.receive(value[1]);
                if (result === process_state.NOMATCH) {
                    this.system.suspend(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, step);
                    });
                }
                else {
                    this.system.schedule(function () {
                        function_scope.system.set_current(function_scope.pid);
                        function_scope.run(machine, machine.next(result));
                    });
                }
            }
            else {
                this.system.schedule(function () {
                    function_scope.system.set_current(function_scope.pid);
                    function_scope.run(machine, machine.next(value));
                });
            }
        }
    }
}

/**
 * Manages all of the processes.
 */
class ProcessSystem {
    constructor(scheduler = new DefaultScheduler(5)) {
        this.pids = new Map();
        this.mailboxes = new Map();
        this.names = new Map();
        this.links = new Map();
        this.monitors = new Map();
        this.current_process = null;
        this.scheduler = scheduler;
        this.suspended = new Map();
        let process_system_scope = this;
        this.main_process_pid = this.spawn(function* () {
            yield process_system_scope.sleep(Symbol.for('Infinity'));
        });
        this.set_current(this.main_process_pid);
    }
    static *run(fun, args, context = null) {
        if (fun.constructor.name === 'GeneratorFunction') {
            return yield* fun.apply(context, args);
        }
        else {
            return yield fun.apply(context, args);
        }
    }
    /**
     * Starts a process represented by the given generator function
     * @param args Either a generator function or a module, function and arguments
     */
    spawn(...args) {
        console.log("SPAAAWN")
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], false, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, false, false).pid;
        }
    }
    /**
     * Starts a process using the generator function from the specified module
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_link(...args) {
        if (args.length === 1) {
            let fun = args[0];
            return this.add_proc(fun, [], true, false).pid;
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            return this.add_proc(mod[fun], the_args, true, false).pid;
        }
    }
    /**
     * links the current process with the process from the given pid
     * @param pid pid of the process to link to
     */
    link(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid);
            const IncomingProcessLink = this.links.get(pid);
            if (currentProcessLink && IncomingProcessLink) {
                currentProcessLink.add(pid);
                IncomingProcessLink.add(currentProcessPid);
            }
        }
    }
    /**
     * unlinks the current process from the process from the given pid
     * @param pid pid of the process to link to
     */
    unlink(pid) {
        const currentProcessPid = this.pid();
        if (currentProcessPid != null) {
            const currentProcessLink = this.links.get(currentProcessPid);
            const IncomingProcessLink = this.links.get(pid);
            if (currentProcessLink && IncomingProcessLink) {
                currentProcessLink.delete(pid);
                IncomingProcessLink.delete(currentProcessPid);
            }
        }
    }
    /**
     * Spawns a process and then monitors it
     * @param args Either a generator function or a module, function and arguments
     */
    spawn_monitor(...args) {
        if (args.length === 1) {
            let fun = args[0];
            let process = this.add_proc(fun, [], false, true);
            return [process.pid, process.monitors[0]];
        }
        else {
            let mod = args[0];
            let fun = args[1];
            let the_args = args[2];
            let process = this.add_proc(mod[fun], the_args, false, true);
            return [process.pid, process.monitors[0]];
        }
    }
    /**
     * Monitors the given process
     * @param pid pid of the process to link to
     */
    monitor(pid) {
        const real_pid = this.pidof(pid);
        const ref = this.make_ref();
        if (this.currentProcess != null) {
            if (real_pid) {
                this.monitors.set(ref, {
                    monitor: this.currentProcess.pid,
                    monitee: real_pid,
                });
                const process = this.pids.get(real_pid);
                if (process) {
                    process.monitors.push(ref);
                }
                return ref;
            }
            else {
                this.send(this.currentProcess.pid, ['unboxed', ['DOWN', ref, pid, real_pid, 'noproc']]);
                return ref;
            }
        }
        return null;
    }
    /**
     * Removes the monitor
     * @param ref Reference to monitor
     */
    demonitor(ref) {
        if (this.monitors.has(ref)) {
            this.monitors.delete(ref);
            return true;
        }
        return false;
    }
    /**
     * Sets the current process
     * @param id PID or name of process
     */
    set_current(id) {
        let pid = this.pidof(id);
        if (pid) {
            const next = this.pids.get(pid);
            if (next) {
                this.current_process = next;
                if (this.currentProcess) {
                    this.currentProcess.status = process_state.RUNNING;
                }
            }
        }
    }
    add_proc(fun, args, linked, monitored) {
        let newproc = new Process(this, fun, args);
        this.pids.set(newproc.pid, newproc);
        this.mailboxes.set(newproc.pid, newproc.mailbox);
        this.links.set(newproc.pid, new Set());
        if (linked) {
            this.link(newproc.pid);
        }
        if (monitored) {
            this.monitor(newproc.pid);
        }
        newproc.start();
        return newproc;
    }
    remove_proc(pid, exitreason) {
        this.pids.delete(pid);
        this.unregister(pid);
        this.scheduler.removePid(pid);
        const linkedPids = this.links.get(pid);
        if (linkedPids) {
            for (let linkpid of linkedPids) {
                this.exit(linkpid, exitreason);
                const linkedPid = this.links.get(linkpid);
                if (linkedPid) {
                    linkedPid.delete(pid);
                }
            }
            this.links.delete(pid);
        }
    }
    /**
     * registers the given name to the pid
     * @param name The name to give the process
     * @param pid The pid of the process
     */
    register(name, pid) {
        if (!this.names.has(name)) {
            this.names.set(name, pid);
        }
        else {
            throw new Error('Name is already registered to another process');
        }
    }
    /**
     * Finds a process by the given name
     * @param name the name of the process
     */
    whereis(name) {
        return this.names.has(name) ? this.names.get(name) : null;
    }
    /**
     * returns the liast of names that are registered
     */
    registered() {
        return this.names.keys();
    }
    /**
     * unregisters the names associated with the pid
     * @param pid The pid of the process
     */
    unregister(pid) {
        for (let name of this.names.keys()) {
            if (this.names.has(name) && this.names.get(name) === pid) {
                this.names.delete(name);
            }
        }
    }
    /**
     * Returns the PID of the current process
     */
    pid() {
        if (this.currentProcess) {
            return this.currentProcess.pid;
        }
        return null;
    }
    /**
     * takes the input and tries to find the pid. Input can be a `pid`, `Process`, or name the pid is associated with
     * @param id The registered name or pid of the process
     */
    pidof(id) {
        if (id instanceof PID) {
            return this.pids.has(id) ? id : null;
        }
        else if (id instanceof Process) {
            return id.pid;
        }
        else {
            let pid = this.whereis(id);
            if (pid === null)
                throw 'Process name not registered: ' + id + ' (' + typeof id + ')';
            return pid;
        }
    }
    /**
     * sends a message the the process represented by the pid
     * @param id
     * @param msg
     */
    send(id, msg) {
        const pid = this.pidof(id);
        if (pid) {
            const mailbox = this.mailboxes.get(pid);
            if (mailbox) {
                mailbox.deliver(msg);
            }
            if (this.suspended.has(pid)) {
                let fun = this.suspended.get(pid);
                this.suspended.delete(pid);
                if (fun) {
                    this.schedule(fun);
                }
            }
        }
        return msg;
    }
    /**
     * Tells the current process to receive a message that the function can handle.
     * If no match then the process is put in the suspended state until a message arrives
     * or the timeout is reached.
     * If the timeout is reached and no msg matches, then the timeoutFn is called
     * @param fun
     * @param timeout
     * @param timeoutFn
     */
    receive(fun, timeout = 0, timeoutFn = () => true) {
        let DateTimeout = null;
        if (timeout === 0 || timeout === Infinity) {
            DateTimeout = null;
        }
        else {
            DateTimeout = Date.now() + timeout;
        }
        return [process_state.RECEIVE, fun, DateTimeout, timeoutFn];
    }
    /**
     * puts the current process to sleep
     * @param duration
     */
    sleep(duration) {
        return [process_state.SLEEP, duration];
    }
    /**
     * Suspends the current process
     * @param fun
     */
    suspend(fun) {
        if (this.currentProcess) {
            this.currentProcess.status = process_state.SUSPENDED;
            this.suspended.set(this.currentProcess.pid, fun);
        }
    }
    /**
     * Makes current process go to sleep
     * @param fun
     * @param time
     */
    delay(fun, time) {
        if (this.currentProcess) {
            this.currentProcess.status = process_state.SLEEPING;
            if (Number.isInteger(time)) {
                this.scheduler.scheduleFuture(this.currentProcess.pid, time, fun);
            }
        }
    }
    /**
     * Schedules execution of a process reduction
     * @param fun
     * @param pid
     */
    schedule(fun, pid) {
        if (this.currentProcess) {
            const the_pid = pid != null ? pid : this.currentProcess.pid;
            this.scheduler.schedule(the_pid, fun);
        }
    }
    /**
     * terminates the current process with the given reason.
     * @param one
     * @param two
     */
    exit(one, two) {
        let pid = null;
        let reason = null;
        let process = null;
        if (two) {
            pid = one;
            reason = two;
            const thePid = this.pidof(pid);
            if (thePid) {
                process = this.pids.get(thePid);
            }
            if (process) {
                if (process.is_trapping_exits() ||
                    reason === process_state.KILL ||
                    reason === process_state.NORMAL) {
                    const mailbox = this.mailboxes.get(process.pid);
                    if (mailbox) {
                        mailbox.deliver(['unboxed', [process_state.EXIT, this.pid(), reason]]);
                    }
                }
                else {
                    process.signal(reason);
                }
            }
        }
        else {
            if (this.currentProcess) {
                pid = this.currentProcess.pid;
                reason = one;
                process = this.currentProcess;
                process.signal(reason);
            }
        }
        if (process) {
            for (let ref of process.monitors) {
                let mons = this.monitors.get(ref);
                if (mons) {
                    this.send(mons['monitor'], ['unboxed', ['DOWN', ref, mons['monitee'], mons['monitee'], reason]]
                        );
                }
            }
        }
    }
    /**
     * terminates the current process with an error
     * @param reason
     */
    error(reason) {
        if (this.currentProcess) {
            this.currentProcess.signal(reason);
        }
    }
    /**
     * Sets flags on the current process.
      - Note: the only flag respected is the `Symbol.for("trap_exit")` flag.
      If value is `true`, then exit signals from linked processes are turned into
      messages and sent to the current processes mailbox.
      If value is `false`, the exit is treated as normal and terminates the process.
      Setting it to `true` is useful for supervising processes.
     * @param args
     */
    process_flag(...args) {
        if (args.length == 2) {
            const flag = args[0];
            const value = args[1];
            if (this.currentProcess) {
                return this.currentProcess.process_flag(flag, value);
            }
        }
        else {
            const pid = this.pidof(args[0]);
            if (pid) {
                const flag = args[1];
                const value = args[2];
                const process = this.pids.get(pid);
                if (process) {
                    return process.process_flag(flag, value);
                }
            }
        }
    }
    /**
     * Adds a value to the current process's dictionary
     * @param key
     * @param value
     */
    put(key, value) {
        if (this.currentProcess) {
            this.currentProcess.dict.set(key, value);
        }
    }
    /**
     * Gets the current process's dictionary
     */
    get_process_dict() {
        if (this.currentProcess) {
            return this.currentProcess.dict;
        }
        throw new Error('No Current Process');
    }
    /**
     * Gets a value from the current process's dictionary or the default if key not in dictionary
     * @param key
     * @param default_value
     */
    get(key, default_value = null) {
        if (this.currentProcess && key in this.currentProcess.dict) {
            return this.currentProcess.dict.get(key);
        }
        else {
            return default_value;
        }
    }
    /**
     * Gets all the keys from the current process's dictionary
     * @param value
     */
    get_keys(value) {
        if (value) {
            let keys = [];
            if (this.currentProcess) {
                for (let key of Object.keys(this.currentProcess.dict)) {
                    if (this.currentProcess.dict.get(key) === value) {
                        keys.push(key);
                    }
                }
            }
            return keys;
        }
        if (this.currentProcess) {
            return Object.keys(this.currentProcess.dict);
        }
        throw new Error('No Current Process');
    }
    /**
     * Removes the key and the associated value from the current process's dictionary
     *
     * If no key is given, removes all entries from the current process's dictionary
     * @param key the key to remove
     */
    erase(key) {
        if (this.currentProcess) {
            if (key != null && this.currentProcess.dict.has(key)) {
                this.currentProcess.dict.delete(key);
            }
            else {
                this.currentProcess.dict = new Map();
            }
        }
    }
    /**
     * Returns if the given pid is alive
     * @param pid
     */
    is_alive(pid) {
        const real_pid = this.pidof(pid);
        return real_pid != null;
    }
    /**
     * Returns a list of all the pids
     */
    list() {
        return Array.from(this.pids.keys());
    }
    /**
     * Returns a unique reference
     */
    make_ref() {
        return new REF();
    }
    get currentProcess() {
        if (this.current_process) {
            return this.current_process;
        }
        return null;
    }
}

var system = new ProcessSystem();
console.log(system.spawn(function* (){
    console.log("P1: ", system.pid())
    return 0;
    }))
console.log(system.spawn(function* (){
    console.log("P2: ", system.pid())
    return 0;
    }))
console.log(system.spawn(function* (){
    console.log("P3: ", system.pid())
    return 0;
    }))

var pid1 = system.spawn(function*() {
  while (true) {
    yield system.receive(function(value) {
      return console.log(value)
    })

    system.send(pid2, 'message from 1')
  }
})

system.register('Sally', pid1)

var pid2 = system.spawn(function*() {
  while (true) {
    system.send('Sally', 'message from 2')

    yield system.receive(function(value) {
      return console.log(value)
    })
  }
})

console.log(system);

/* FFI CODE */
function do_apply_4(moduleName) {
        return function(functionName) {
            return function(argumentArray) {
                return function(failCallback) {
                    var module = undefined;
                    var f = undefined;
                    try {
                        let name = moduleName.replace(/\w\S*/g, function(txt){return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();});
                        try {
                            module = PS[name];
                        } catch(e) {
                            module = require("../"+name+"/index.js");
                        }
                        f = module["erlps__" + functionName + "__" + argumentArray.length]
                    } catch(e) {
                        module = undefined;
                        f = undefined;
                    }
                    if (module !== undefined && f !== undefined) {
                        return f()(argumentArray);
                    } else {
                        failCallback()
                    }
                }
            }
        }
    };

    return {
        do_apply_4: do_apply_4,
        system: system
    }

}();

console.log(RUNTIME);

exports.do_apply_4 = RUNTIME.do_apply_4;
