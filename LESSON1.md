Lesson 1. Code vs configuration.
========

Designing a robust way to consume external configuration.

# Preface
Observations below apply only to software deployed in a cloud, when a
developer has access to both code and configuration. On-premise software 
needs configuration for completely different reasons.

## Change management
*In information technology and computer science, a program is described 
as stateful if it is designed to remember preceding events or user 
interactions; the remembered information is called the state of the system.*

### System state transition

Time cannot go back. State change of a system with side effects cannot
be rolled back. If the system in the new state fails to meet 
requirements, it needs to be transitioned to another state where it 
meets requirements again. 

For example, start 'ping' command, that enters stable state #1 printing
"Reply from ...". Then disable a NICs, so 'ping' enters state #2, 
printing "Transmit failure". Enabling NIC brings 'ping' to state #3,
printing "Reply from ..." again. From the user point of view state #1 
and #3 are identical, as if the system has recovered.

A TCP connection exhibits different behaviour. When it  experiences 
fault, action is taken depending on the nature of the fault. System 
tries to get to good state for transient errors and shuts down if error 
is persistent.

*An error is persistent if the frequency of occurrence of the associated
fault exceeds some predetermined threshold.* \["Reliable Computer 
Systems: Design and Evaluatuion", Daniel Siewiorek, ‎Robert Swarz\]

Persistent failure must be handled by a supervising entity, for example,
a human restarting telnet client.

**When system is not ready to handle failure, it's up to supervisor
to recover the system.**

### Signals

*Evolution is change in the heritable characteristics of biological 
populations over successive generations.*
 Hall & Hallgrímsson, "Strickberger's Evolution (4th ed.)."
 
Evolutionary principle applies to changes in software. Successful change 
propagates to entire system, evolving it to the next generation. Change 
that introduces failure should be *detected* and, if possible, 
abandoned before it propagates.

(Illustration)

Detection mechanisms rely on evidences, or **signals**, including, but 
not limited to: 
* \[scientific\] validation
* tests and experiments
* reviews
* historical evidences
* confidence

Amount of signal to collect may be enforced (mandatory reviews, required
test coverage, a/b testing for a specified period, staged rollout).
In most cases enforcement depends on the anticipated consequences. An 
example, for a change in documentation confidence may be enough. However 
adding comment line to Erlang source file may need a review and
justification, as, incidentally, it [leads to a change in the 
system!](#fun-facts)


## What is configuration? 
And how it is different to source code

1. Environment: hostname, operating system, kernel version, VM version,
geographical location, hardware capabilities. Available only at runtime.
Safe defaults may be available, changes are not expected.
2. API for external consumption, usually in a different language: user 
preferences, feature flags (including kill switches, knobs, gatekeepers,
guards, long running a/b experiments). Safe defaults are always 
available, changes are expected.
3. Internal API exposed to bypass signal collection stages: settings 
assuming knowledge of system internals. Also includes code written in a
different language and auxiliary databases, often available at build
time, but consumed in runtime. 

### Environment
Settings that are expected to remain static during the entire runtime
session. System does not expect any changes and is not designed to
handle the change. For example, Windows 10 is not able to use 
hot-plugged CPU, as it assumes CPU count constant through the entire
session, until rebooted.

However, Windows 2016 Server is able to detect and use these. Which 
turns CPU count setting into second category - ["API for external 
consumption"](#api-for-external-consumption).

Handling of environment configuration changes is not covered in this 
lesson.

### API for external consumption
The system is expected to change behaviour to reflect a change in 
configuration. Change request comes from potentially untrusted source,
so it must be validated before proceeding, and rejected if it is not 
valid. A good example of this behaviour is Apache web server, refusing 
to reload invalid configuration.

Configuration change must not be persisted until system reached stable 
state. See [Configuration as a secondary code delivery 
pipeline](#configuration-as-a-secondary-code-delivery-pipeline)
and ["Consuming externally supplied 
changes"](#exercise-2-consuming-externally-supplied-changes) 
exercise below.

Bootstrapping problem, when the system never reaches stable state,
and thus unable to reject the very first change, is always persistent 
failure that cannot be recovered by the system itself. It must be 
resolved by the supervising entity.

API for external consumption must be documented and supported, and 
should maintain backward compatibility.

### Internal API
Signal collection stages required to make a change may become cumbersome
and counter-productive. Urgent business needs may justify exposing 
system internals in an unsafe way, deliberately introducing a backdoor.

This approach is usually discouraged, as it leads to technical debt 
accumulation, unexpected dependencies, potential security issues and
hair loss. To ensure uninterrupted service, most organisations enforce
some form of signal collection when internal APIs are used to make a 
change to the system. Ultimately, it leads to treating configuration 
the same way as code, but expressed in a different language.

There may be other reasons to expose internal APIs via configuration 
language. For example, configuration delivery pipeline (CDP) may be 
more mature and robust than code delivery one. That, however, raises 
a question what should be considered code, and what is configuration.

## Configuration as code? Configuration is code!

[Configuration as code is the formal migration of config between 
environments, backed by a version control system.
](https://rollout.io/blog/configuration-as-code-everything-need-know/)

Configuration **is** code, often expressed in a different language. For 
example, Java software may consume XM**L** configuration. Erlang code
may rely on JSON-formatted files. Complexity of configuration definition
language may reach a level when configuration itself becomes too 
difficult to maintain, and *configuration complexity clock* makes a full
turn.

[Configuration Complexity Clock](http://mikehadlow.blogspot.com/2012/05/configuration-complexity-clock.html)

*In the pub after work someone quips, “we’re back where we started four 
years ago, hard coding everything, except now in a much crappier 
language.”*

Most of "internal API" configuration is code. Using code deployment 
techniques for updating this configuration should be preferred to 
writing it using a different DSL in a different repository.

## Configuration as a secondary code delivery pipeline

As mentioned in [Internal API](#internal-api), configuration delivery
pipeline may be more reliable and robust than the primary (code) 
delivery pipeline. However introducing secondary pipeline is not as 
straightforward as it may look:
* code delivered as configuration needs to be correctly merged into
running code, or rejected, if it cannot be merged
* dependencies between code & configuration must be supported and 
properly maintained, including backward compatibility
* delivery pipeline must be implemented, supported and maintained
* delivery pipeline must define and observe consistent SLA

See ["Consuming externally supplied 
changes"](#exercise-2-consuming-externally-supplied-changes) 
exercise 2 for an example of following this process.

# Exercise 1. Configuration is code, as in Erlang OTP 
Official Erlang history starts in 1988, when it [first escaped the 
Ericsson labs](https://www.erlang.org/course/history). It took nearly
10 years to open source Erlang and OTP, Open Telecom Platform. The 
language is nearly as old as C++ (first mentions in 1978 and first
standard released in 1998). It proved mature enough to support Ericsson
broadband, ATM and GPRS solutions in 1996. Design patterns provided by
OTP are battle-tested and stood the test of time. 

Original designation of the language (embedded systems) heavily 
influenced design decisions. Providing two separate delivery pipelines 
(code & configuration) is not trivial. Thus, traditionally, for 
Erlang/OTP, configuration change is code change. 

## Using OTP releases to deliver configuration changes

Source code: [OTP Releases Example](https://github.com/max-au/ex1_relup.git)

Erlang release, containing a single application with one variable to
configure. Tags 0.1.0, 0.2.0 and 0.3.0.

### ex1_relup example
This simple example demonstrates approach taken by Erlang/OTP design.
First version of the system prints a pre-configured text every second.
Upgrade to second version changes the only configuration parameter 
available, delay between printed messages. Third version shows that
same effect is reached without using configuration variable, and 
resulting code is easier to read and understand. While update procedure
is not any different from the previous one.

### Useful patterns
* if a configuration variable is mentioned once, and only in one module, 
and is not designed as an external API (e.g. validated, documented, 
supported, and keeps backward compatible behaviour), it should be
replaced with actual value, it it's self-explaining (e.g. atom named
'yesterday'), or a macro definition, if is not (SECONDS_IN_A_DAY is
better than 86400).
* if a configuration variable used multiple times, but only in one 
module, it should be replaced with macro definition, holding the value
* *library* applications should avoid using application configuration
altogether, see [Elixir library 
anti-patterns](https://hexdocs.pm/elixir/master/library-guidelines.html#anti-patterns),
as it becomes impossible for two dependencies to use your library in 
two different ways.
* configuration values used from multiple modules may be moved to
a separate module providing stable interface, if is has the same
usage semantics and validation rules (it may then be replaced with an 
actual value, see first practice)



# Exercise 2. Consuming externally supplied changes

Prerequisites: 
* [Who Supervises The Supervisors?
](https://learnyousomeerlang.com/supervisors)
* [It's about the guarantees](https://ferd.ca/it-s-about-the-guarantees.html)


Same application is used to demonstrate the concept. It's an art of
live upgrade, after all.

Source code: [OTP Releases Example](https://github.com/max-au/ex1_relup.git)

Tag 0.4.0 introduces separate configuration delivery pipeline. External
utility used to simulate unreliable delivery.

## Configuration delivery pipeline (CDP)
See [Configuration as a secondary code delivery pipeline
](#configuration-as-a-secondary-code-delivery-pipeline) for a brief 
explanation.

### Merging delivered configuration
As explained above, configuration is code written in a different 
language, often domain-specific. For this example, persistent 
configuration is stored in Erlang term format, presented as text,
and changes are delivered as text lines with OS-specific delimiters,
using following format:

```SUB:MOD:KEY:VALUE```

Here SUB stands for subscription name, MOD for module name, KEY for key 
and VALUE for value. SUB, MOD and KEY must be existing atoms to prevent
atom space exhaustion.

Example:
```any:ex1_relup:ticker:2000```

Note: subscriptions are introduced only to demonstrate state recovery
technique, required to support stateful CDP implementations. This
exercise uses a single subscription key.

### Dependencies between code and configuration
Design of a generic language-agnostic system that handles dependencies
between different repositories (code & configuration), written in
different languages and probably lacking any support for dependency
management is beyond the scope of the exercise. Therefore developer
confidence will be the only enforced signal to submit a change.

### Delivery pipeline design
Initial configuration is supplied as a single file, ```/tmp/cdp```,
written in Erlang term format. 

Updates are delivered using subscription model. Authoring a change to 
configuration is done using 

```echo A:B:C:D' >> /tmp/cdp.new```

Changes rejected are appended to ```/tmp/cdp.rej```, persisted changes 
are appended to ```/tmp/cdp``` without SUBSCRIPTION and MODULE parts.

### Service Level Agreement
For this exercise:
* CDP guarantees configuration availability for system startup, and 
configuration is guaranteed to be valid
* CDP requires system to *subscribe* before changes are delivered,
providing a subscription key
* CDP does not guarantee validity of delivered change, but expects
the system to reject invalid changes to stop propagation of this 
change
* CDP guarantees to keep subscriptions for downtime windows shorter
than 10 seconds
* CDP guarantees to have no more than 5 downtime windows when 
subscriptions are lost for any 10 minute period 

System cannot continue if any guarantee is broken, and it is up to
supervising entity to recover the system.

### Runtime design

To simulate unreliable connection, this exercise uses Unix "tail" to 
watch file changes. Short transient failure is simulated with
```pkill tail```.

To simulate prolonged failure, use 
 
```sudo chmod -x `which tail` ; sleep 30; sudo chmod +x `which tail` ```

First clause of [SLA above](#service-level-agreement) declares hard 
dependency of code on configuration during bootstrap time. Therefore 
```file:consult()``` must not be wrapped into ```try ... catch```. 
System must *guarantee* configuration availability before starting any
services, so consult() is done in gen_server:Init() callback.

For complex systems, layered approach may be used, and  only minimal set
of configuration values is expected to be available at boot time.

Second clause requires system to *expect* and reject invalid change
requests.

Third clause requires system to *expect* connection going down for up to
10 seconds without losing CDP state, and it also suggests that after 10
seconds CDP state is lost and must be recovered upon reconnection.

Fourth clause requires system to shut down if CDP is flaky and goes
down too often.

Module ex1_relup that consumes configuration value depends on 
configuration and subscription manager availability. Configuration 
itself does not depend on ex1_relup, and in case when ex1_relup fails, 
configuration remains unchanged. However subscription manager is 
expected to automatically stop a subscription if subscriber is shut 
down.

(Dependency Diagram)

### Suggested improvements

* remove requirement to expose module name in configuration change
* allow multiple modules to run validation logic for the same key 
(useful when the same key reused for various purposes)
* alternatively, require all configuration keys to be functions

# Fun Facts

### Adding comment line is not a no-op change

* adding a line with comments into *.erl file changes line numbers, thus
triggering code upgrade even if no actual code was changed. Changing
the comment line, however, does not have this effect, because line
numbers in call stacks are left unchanged
