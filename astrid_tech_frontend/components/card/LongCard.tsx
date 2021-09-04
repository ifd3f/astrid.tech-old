import { FC, PropsWithChildren, ReactNode } from "react";
import * as vs from "./values";

export const LongCard: FC = () => {
  const fields = [
    vs.pronouns,
    vs.timezone,
    vs.phone,
    vs.email,
    vs.birthday,
    vs.github,
    vs.linkedin,
    vs.favOS,
  ];
  return (
    <div className="h-card">
      <h2 className="p-name">Astrid Yu</h2>
      <table>
        <tbody>
          {fields.map((f) => (
            <tr>
              <th style={{ textAlign: "right", paddingRight: 10 }}>
                {f.title}
              </th>
              <td>{f.children}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default LongCard;
