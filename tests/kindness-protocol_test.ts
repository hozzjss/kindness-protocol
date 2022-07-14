import {
  Clarinet,
  Tx,
  Chain,
  Account,
  types,
} from "https://deno.land/x/clarinet@v0.31.0/index.ts";
import { assertEquals } from "https://deno.land/std@0.90.0/testing/asserts.ts";
import * as mod from "https://deno.land/std@0.76.0/node/buffer.ts";

Clarinet.test({
  name: "Ensure that <...>",
  fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get("deployer")!;
    const wallet_1 = accounts.get("wallet_1")!;
    const contractName = deployer.address + ".kindness-protocol";
    let block = chain.mineBlock([
      Tx.contractCall(
        contractName,
        "mint",
        [
          types.none(),
          types.ascii(
            "https://images.gamma.io/ipfs/Qme9rxy7Eq6ro9GscFNC7D95aQSwzMwapr9EE98qzEZs6h"
          ),
          types.some(types.principal(wallet_1.address)),
        ],
        deployer.address
      ),
      Tx.contractCall(
        contractName,
        "mint",
        [
          types.some("0x00000000000000000000000000000000"),
          types.ascii(
            "https://images.gamma.io/ipfs/Qme9rxy7Eq6ro9GscFNC7D95aQSwzMwapr9EE98qzEZs6h"
          ),
          types.none(),
        ],
        deployer.address
      ),
      Tx.contractCall(
        contractName,
        "mint",
        [
          types.some(
            "0x0000000000000000000000000000000000000000000000000000000000000000"
          ),
          types.ascii(
            "https://images.gamma.io/ipfs/Qme9rxy7Eq6ro9GscFNC7D95aQSwzMwapr9EE98qzEZs6h"
          ),
          types.none(),
        ],
        deployer.address
      ),
      /*
       * Add transactions with:
       * Tx.contractCall(...)
       */
    ]);
    console.log(block.receipts.map(({ events }) => events));
    assertEquals(block.receipts.length, 3);
    assertEquals(block.height, 2);

    block = chain.mineBlock([
      /*
       * Add transactions with:
       * Tx.contractCall(...)
       */
    ]);
    assertEquals(block.receipts.length, 0);
    assertEquals(block.height, 3);
  },
});
