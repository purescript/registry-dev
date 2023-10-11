import { init } from 'z3-solver';

export const newZ3Impl = () => {
  console.log("JS: Initing");
  return init().then((z3) => z3);
}

export const newSolverImpl = ({Context}, name) => {
  console.log("Getting new context " + name);
  return new Context(name);
};

export const addVariablesImpl = (Z3, vs) => {
  let newVars = {};
  vs.forEach(({ typ, name }) => {
    let v = null;
    switch (typ) {
      case "int":
        v = Z3.Int.const(name);
        break;
      case "bool":
        v = Z3.Bool.const(name);
        break;
      default:
        throw Error("UNMATCHED: " + typ + " " + name);
    }
    newVars[name] = v;
  });
  return newVars;
};

// https://www.philipzucker.com/z3-rise4fun/optimization.html
export const solveImpl = (Z3, vs, expr) => {
  let solver = new Z3.Optimize();
  let converted = convertExpr(Z3, vs, expr);
  solver.add(converted);
  let goal = mkGoal(vs);
  if (goal !== null) {
    console.log("Calling maximize..");
    solver.maximize(goal);
  }
  //console.log("Solver state");
  console.log(solver.toString());
  //console.log("Checking solver..");
  return solver.check()
    .then((checkResult) => {
      if (checkResult === "unsat") {
        // TODO: expose unsat core
        throw Error("UNSAT!");
      } else {
        let model = solver.model();
        console.log("------------------------------------------------------------------------");
        let modelResults = {}
        model.decls().forEach(element => {
          let v = model.get(element);
          // TODO: the value function is only for Ints, but not bools?
          if (typeof v.value === 'function') {
            modelResults[element.name()] = Number(v.value());
          }
        });
        return modelResults;
      }
    });
}

const convertExpr = (Z3, vs, e) => {
  switch (e.op) {
    case "or":
      let newEs1 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.Or(...newEs1);
    case "and":
      let newEs2 = e.l.map((clause) => convertExpr(Z3, vs, clause));
      return Z3.And(...newEs2);
    case "implies":
      return Z3.Implies(convertExpr(Z3, vs, e.l), convertExpr(Z3, vs, e.r));
    case "iff":
      return Z3.Eq(vs[e.l], convertExpr(Z3, vs, e.r));
    case "eq":
      return vs[e.l].eq(e.r);
    case "ge":
      return vs[e.l].ge(e.r);
    case "lt":
      return vs[e.l].lt(e.r);
    case "var":
      return vs[e.l];
    case "tru":
      return true;

    default:
      throw Error("UNMATCHED Z3 EXPR");
  }
}

const mkGoal = (vs) => {
  let goal = null;
  let entries = Object.entries(vs);
  entries.forEach(([key, value]) => {
    if (!key.startsWith("var__")) {
      console.log("Adding to maximise:", key);
      if (goal == null) {
        goal = value;
      } else {
        console.log("Adding", key, "to", goal.toString())
        goal = value.add(goal);
      }
    }
  });
  return goal;
}
